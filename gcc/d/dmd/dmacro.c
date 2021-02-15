
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/macro.c
 */

/* Simple macro text processor.
 */

#include "root/dsystem.h"

#include "mars.h"
#include "errors.h"
#include "root/rmem.h"
#include "root/root.h"

#include "macro.h"

bool isIdStart(const utf8_t *p);
bool isIdTail(const utf8_t *p);
int utfStride(const utf8_t *p);

utf8_t *memdup(const utf8_t *p, size_t len)
{
    return (utf8_t *)memcpy(mem.xmalloc(len), p, len);
}

Macro::Macro(const utf8_t *name, size_t namelen, const utf8_t *text, size_t textlen)
{
    next = NULL;

    this->name = name;
    this->namelen = namelen;

    this->text = text;
    this->textlen = textlen;
    inuse = 0;
}


Macro *Macro::search(const utf8_t *name, size_t namelen)
{   Macro *table;

    //printf("Macro::search(%.*s)\n", namelen, name);
    for (table = this; table; table = table->next)
    {
        if (table->namelen == namelen &&
            memcmp(table->name, name, namelen) == 0)
        {
            //printf("\tfound %d\n", table->textlen);
            break;
        }
    }
    return table;
}

Macro *Macro::define(Macro **ptable, const utf8_t *name, size_t namelen, const utf8_t *text, size_t textlen)
{
    //printf("Macro::define('%.*s' = '%.*s')\n", namelen, name, textlen, text);

    Macro *table;

    //assert(ptable);
    for (table = *ptable; table; table = table->next)
    {
        if (table->namelen == namelen &&
            memcmp(table->name, name, namelen) == 0)
        {
            table->text = text;
            table->textlen = textlen;
            return table;
        }
    }
    table = new Macro(name, namelen, text, textlen);
    table->next = *ptable;
    *ptable = table;
    return table;
}

/**********************************************************
 * Given buffer p[0..end], extract argument marg[0..marglen].
 * Params:
 *      n       0:      get entire argument
 *              1..9:   get nth argument
 *              -1:     get 2nd through end
 */

size_t extractArgN(const utf8_t *p, size_t end, const utf8_t **pmarg, size_t *pmarglen, int n)
{
    /* Scan forward for matching right parenthesis.
     * Nest parentheses.
     * Skip over "..." and '...' strings inside HTML tags.
     * Skip over <!-- ... --> comments.
     * Skip over previous macro insertions
     * Set marglen.
     */
    unsigned parens = 1;
    unsigned char instring = 0;
    unsigned incomment = 0;
    unsigned intag = 0;
    unsigned inexp = 0;
    int argn = 0;

    size_t v = 0;

  Largstart:
    // Skip first space, if any, to find the start of the macro argument
    if (n != 1 && v < end && isspace(p[v]))
        v++;
    *pmarg = p + v;

    for (; v < end; v++)
    {   utf8_t c = p[v];

        switch (c)
        {
            case ',':
                if (!inexp && !instring && !incomment && parens == 1)
                {
                    argn++;
                    if (argn == 1 && n == -1)
                    {   v++;
                        goto Largstart;
                    }
                    if (argn == n)
                        break;
                    if (argn + 1 == n)
                    {   v++;
                        goto Largstart;
                    }
                }
                continue;

            case '(':
                if (!inexp && !instring && !incomment)
                    parens++;
                continue;

            case ')':
                if (!inexp && !instring && !incomment && --parens == 0)
                {
                    break;
                }
                continue;

            case '"':
            case '\'':
                if (!inexp && !incomment && intag)
                {
                    if (c == instring)
                        instring = 0;
                    else if (!instring)
                        instring = c;
                }
                continue;

            case '<':
                if (!inexp && !instring && !incomment)
                {
                    if (v + 6 < end &&
                        p[v + 1] == '!' &&
                        p[v + 2] == '-' &&
                        p[v + 3] == '-')
                    {
                        incomment = 1;
                        v += 3;
                    }
                    else if (v + 2 < end &&
                        isalpha(p[v + 1]))
                        intag = 1;
                }
                continue;

            case '>':
                if (!inexp)
                    intag = 0;
                continue;

            case '-':
                if (!inexp &&
                    !instring &&
                    incomment &&
                    v + 2 < end &&
                    p[v + 1] == '-' &&
                    p[v + 2] == '>')
                {
                    incomment = 0;
                    v += 2;
                }
                continue;

            case 0xFF:
                if (v + 1 < end)
                {
                    if (p[v + 1] == '{')
                        inexp++;
                    else if (p[v + 1] == '}')
                        inexp--;
                }
                continue;

            default:
                continue;
        }
        break;
    }
    if (argn == 0 && n == -1)
        *pmarg = p + v;
    *pmarglen = p + v - *pmarg;
    //printf("extractArg%d('%.*s') = '%.*s'\n", n, end, p, *pmarglen, *pmarg);
    return v;
}


/*****************************************************
 * Expand macro in place in buf.
 * Only look at the text in buf from start to end.
 */

void Macro::expand(OutBuffer *buf, size_t start, size_t *pend,
        const utf8_t *arg, size_t arglen)
{
    // limit recursive expansion
    static int nest;
    if (nest > global.recursionLimit)
    {
        error(Loc(), "DDoc macro expansion limit exceeded; more than %d expansions.",
              global.recursionLimit);
        return;
    }
    nest++;

    size_t end = *pend;
    assert(start <= end);
    assert(end <= buf->length());

    /* First pass - replace $0
     */
    arg = memdup(arg, arglen);
    for (size_t u = start; u + 1 < end; )
    {
        utf8_t *p = (utf8_t *)buf->slice().ptr;   // buf->slice().ptr is not loop invariant

        /* Look for $0, but not $$0, and replace it with arg.
         */
        if (p[u] == '$' && (isdigit(p[u + 1]) || p[u + 1] == '+'))
        {
            if (u > start && p[u - 1] == '$')
            {   // Don't expand $$0, but replace it with $0
                buf->remove(u - 1, 1);
                end--;
                u += 1; // now u is one past the closing '1'
                continue;
            }

            utf8_t c = p[u + 1];
            int n = (c == '+') ? -1 : c - '0';

            const utf8_t *marg;
            size_t marglen;
            if (n == 0)
            {
                marg = arg;
                marglen = arglen;
            }
            else
                extractArgN(arg, arglen, &marg, &marglen, n);
            if (marglen == 0)
            {   // Just remove macro invocation
                //printf("Replacing '$%c' with '%.*s'\n", p[u + 1], marglen, marg);
                buf->remove(u, 2);
                end -= 2;
            }
            else if (c == '+')
            {
                // Replace '$+' with 'arg'
                //printf("Replacing '$%c' with '%.*s'\n", p[u + 1], marglen, marg);
                buf->remove(u, 2);
                buf->insert(u, marg, marglen);
                end += marglen - 2;

                // Scan replaced text for further expansion
                size_t mend = u + marglen;
                expand(buf, u, &mend, NULL, 0);
                end += mend - (u + marglen);
                u = mend;
            }
            else
            {
                // Replace '$1' with '\xFF{arg\xFF}'
                //printf("Replacing '$%c' with '\xFF{%.*s\xFF}'\n", p[u + 1], marglen, marg);
                buf->slice().ptr[u] = 0xFF;
                buf->slice().ptr[u + 1] = '{';
                buf->insert(u + 2, marg, marglen);
                buf->insert(u + 2 + marglen, (const char *)"\xFF}", 2);
                end += -2 + 2 + marglen + 2;

                // Scan replaced text for further expansion
                size_t mend = u + 2 + marglen;
                expand(buf, u + 2, &mend, NULL, 0);
                end += mend - (u + 2 + marglen);
                u = mend;
            }
            //printf("u = %d, end = %d\n", u, end);
            //printf("#%.*s#\n", end, &buf->slice().ptr[0]);
            continue;
        }

        u++;
    }

    /* Second pass - replace other macros
     */
    for (size_t u = start; u + 4 < end; )
    {
        utf8_t *p = (utf8_t *)buf->slice().ptr;   // buf->slice().ptr is not loop invariant

        /* A valid start of macro expansion is $(c, where c is
         * an id start character, and not $$(c.
         */
        if (p[u] == '$' && p[u + 1] == '(' && isIdStart(p+u+2))
        {
            //printf("\tfound macro start '%c'\n", p[u + 2]);
            utf8_t *name = p + u + 2;
            size_t namelen = 0;

            const utf8_t *marg;
            size_t marglen;

            size_t v;
            /* Scan forward to find end of macro name and
             * beginning of macro argument (marg).
             */
            for (v = u + 2; v < end; v+=utfStride(p+v))
            {

                if (!isIdTail(p+v))
                {   // We've gone past the end of the macro name.
                    namelen = v - (u + 2);
                    break;
                }
            }

            v += extractArgN(p + v, end - v, &marg, &marglen, 0);
            assert(v <= end);

            if (v < end)
            {   // v is on the closing ')'
                if (u > start && p[u - 1] == '$')
                {   // Don't expand $$(NAME), but replace it with $(NAME)
                    buf->remove(u - 1, 1);
                    end--;
                    u = v;      // now u is one past the closing ')'
                    continue;
                }

                Macro *m = search(name, namelen);

                if (!m)
                {
                    static const char undef[] = "DDOC_UNDEFINED_MACRO";
                    m = search((const utf8_t *)undef, strlen(undef));
                    if (m)
                    {
                        // Macro was not defined, so this is an expansion of
                        //   DDOC_UNDEFINED_MACRO. Prepend macro name to args.
                        // marg = name[ ] ~ "," ~ marg[ ];
                        if (marglen)
                        {
                            utf8_t *q = (utf8_t *)mem.xmalloc(namelen + 1 + marglen);
                            assert(q);
                            memcpy(q, name, namelen);
                            q[namelen] = ',';
                            memcpy(q + namelen + 1, marg, marglen);
                            marg = q;
                            marglen += namelen + 1;
                        }
                        else
                        {
                            marg = name;
                            marglen = namelen;
                        }
                    }
                }

                if (m)
                {
                    if (m->inuse && marglen == 0)
                    {   // Remove macro invocation
                        buf->remove(u, v + 1 - u);
                        end -= v + 1 - u;
                    }
                    else if (m->inuse &&
                             ((arglen == marglen && memcmp(arg, marg, arglen) == 0) ||
                              (arglen + 4 == marglen &&
                               marg[0] == 0xFF &&
                               marg[1] == '{' &&
                               memcmp(arg, marg + 2, arglen) == 0 &&
                               marg[marglen - 2] == 0xFF &&
                               marg[marglen - 1] == '}'
                              )
                             )
                            )
                    {
                        /* Recursive expansion:
                         *   marg is same as arg (with blue paint added)
                         * Just leave in place.
                         */
                    }
                    else
                    {
                        //printf("\tmacro '%.*s'(%.*s) = '%.*s'\n", m->namelen, m->name, marglen, marg, m->textlen, m->text);
                        marg = memdup(marg, marglen);
                        // Insert replacement text
                        buf->spread(v + 1, 2 + m->textlen + 2);
                        buf->slice().ptr[v + 1] = 0xFF;
                        buf->slice().ptr[v + 2] = '{';
                        memcpy(buf->slice().ptr + v + 3, m->text, m->textlen);
                        buf->slice().ptr[v + 3 + m->textlen] = 0xFF;
                        buf->slice().ptr[v + 3 + m->textlen + 1] = '}';

                        end += 2 + m->textlen + 2;

                        // Scan replaced text for further expansion
                        m->inuse++;
                        size_t mend = v + 1 + 2+m->textlen+2;
                        expand(buf, v + 1, &mend, marg, marglen);
                        end += mend - (v + 1 + 2+m->textlen+2);
                        m->inuse--;

                        buf->remove(u, v + 1 - u);
                        end -= v + 1 - u;
                        u += mend - (v + 1);
                        mem.xfree(const_cast<utf8_t *>(marg));
                        //printf("u = %d, end = %d\n", u, end);
                        //printf("#%.*s#\n", end - u, &buf->slice().ptr[u]);
                        continue;
                    }
                }
                else
                {
                    // Replace $(NAME) with nothing
                    buf->remove(u, v + 1 - u);
                    end -= (v + 1 - u);
                    continue;
                }
            }
        }
        u++;
    }
    mem.xfree(const_cast<utf8_t *>(arg));
    *pend = end;
    nest--;
}
