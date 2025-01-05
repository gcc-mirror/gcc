/**
 * Text macro processor for Ddoc.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dmacro.d, _dmacro.d)
 * Documentation:  https://dlang.org/phobos/dmd_dmacro.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dmacro.d
 */

module dmd.dmacro;

import core.stdc.ctype;
import core.stdc.string;
import dmd.common.outbuffer;
import dmd.root.rmem;

struct MacroTable
{
    /**********************************
     * Define name=text macro.
     * If macro `name` already exists, replace the text for it.
     * Params:
     *  name = name of macro
     *  text = text of macro
     */
    void define(const(char)[] name, const(char)[] text) nothrow pure @safe
    {
        //printf("MacroTable::define('%.*s' = '%.*s')\n", cast(int)name.length, name.ptr, text.length, text.ptr);
        if (auto table = name in mactab)
        {
            (*table).text = text;
            return;
        }
        mactab[name] = new Macro(name, text);
    }

    alias fp_t = bool function(const(char)* p) @nogc nothrow pure;

    /*****************************************************
     * Look for macros in buf and expand them in place.
     * Only look at the text in buf from start to pend.
     *
     * Returns: `true` on success, `false` when the recursion limit was reached
     */
    bool expand(ref OutBuffer buf, size_t start, ref size_t pend, const(char)[] arg, int recursionLimit,
        fp_t isIdStart, fp_t isIdTail) nothrow pure
    {
        version (none)
        {
            printf("Macro::expand(buf[%d..%d], arg = '%.*s')\n", start, pend, cast(int)arg.length, arg.ptr);
            printf("Buf is: '%.*s'\n", cast(int)(pend - start), buf.data + start);
        }
        // limit recursive expansion
        recursionLimit--;
        if (recursionLimit < 0)
            return false;

        size_t end = pend;
        assert(start <= end);
        assert(end <= buf.length);
        /* First pass - replace $0
         */
        arg = memdup(arg);
        for (size_t u = start; u + 1 < end;)
        {
            char* p = cast(char*)buf[].ptr; // buf.data is not loop invariant
            /* Look for $0, but not $$0, and replace it with arg.
             */
            if (p[u] == '$' && (isdigit(p[u + 1]) || p[u + 1] == '+'))
            {
                if (u > start && p[u - 1] == '$')
                {
                    // Don't expand $$0, but replace it with $0
                    buf.remove(u - 1, 1);
                    end--;
                    u += 1; // now u is one past the closing '1'
                    continue;
                }
                char c = p[u + 1];
                int n = (c == '+') ? -1 : c - '0';
                const(char)[] marg;
                if (n == 0)
                {
                    marg = arg;
                }
                else
                    extractArgN(arg, marg, n);
                if (marg.length == 0)
                {
                    // Just remove macro invocation
                    //printf("Replacing '$%c' with '%.*s'\n", p[u + 1], cast(int)marg.length, marg.ptr);
                    buf.remove(u, 2);
                    end -= 2;
                }
                else if (c == '+')
                {
                    // Replace '$+' with 'arg'
                    //printf("Replacing '$%c' with '%.*s'\n", p[u + 1], cast(int)marg.length, marg.ptr);
                    buf.remove(u, 2);
                    buf.insert(u, marg);
                    end += marg.length - 2;
                    // Scan replaced text for further expansion
                    size_t mend = u + marg.length;
                    const success = expand(buf, u, mend, null, recursionLimit, isIdStart, isIdTail);
                    if (!success)
                        return false;
                    end += mend - (u + marg.length);
                    u = mend;
                }
                else
                {
                    // Replace '$1' with '\xFF{arg\xFF}'
                    //printf("Replacing '$%c' with '\xFF{%.*s\xFF}'\n", p[u + 1], cast(int)marg.length, marg.ptr);
                    ubyte[] slice = cast(ubyte[])buf[];
                    slice[u] = 0xFF;
                    slice[u + 1] = '{';
                    buf.insert(u + 2, marg);
                    buf.insert(u + 2 + marg.length, "\xFF}");
                    end += -2 + 2 + marg.length + 2;
                    // Scan replaced text for further expansion
                    size_t mend = u + 2 + marg.length;
                    const success = expand(buf, u + 2, mend, null, recursionLimit, isIdStart, isIdTail);
                    if (!success)
                        return false;
                    end += mend - (u + 2 + marg.length);
                    u = mend;
                }
                //printf("u = %d, end = %d\n", u, end);
                //printf("#%.*s#\n", cast(int)end, &buf.data[0]);
                continue;
            }
            u++;
        }
        /* Second pass - replace other macros
         */
        for (size_t u = start; u + 4 < end;)
        {
            char* p = cast(char*)buf[].ptr; // buf.data is not loop invariant
            /* A valid start of macro expansion is $(c, where c is
             * an id start character, and not $$(c.
             */
            if (p[u] == '$' && p[u + 1] == '(' && isIdStart(p + u + 2))
            {
                //printf("\tfound macro start '%c'\n", p[u + 2]);
                char* name = p + u + 2;
                size_t namelen = 0;
                const(char)[] marg;
                size_t v;
                /* Scan forward to find end of macro name and
                 * beginning of macro argument (marg).
                 */
                for (v = u + 2; v < end; v += utfStride(p[v]))
                {
                    if (!isIdTail(p + v))
                    {
                        // We've gone past the end of the macro name.
                        namelen = v - (u + 2);
                        break;
                    }
                }
                v += extractArgN(p[v .. end], marg, 0);
                assert(v <= end);
                if (v < end)
                {
                    // v is on the closing ')'
                    if (u > start && p[u - 1] == '$')
                    {
                        // Don't expand $$(NAME), but replace it with $(NAME)
                        buf.remove(u - 1, 1);
                        end--;
                        u = v; // now u is one past the closing ')'
                        continue;
                    }
                    Macro* m = search(name[0 .. namelen]);
                    if (!m)
                    {
                        immutable undef = "DDOC_UNDEFINED_MACRO";
                        m = search(undef);
                        if (m)
                        {
                            // Macro was not defined, so this is an expansion of
                            //   DDOC_UNDEFINED_MACRO. Prepend macro name to args.
                            // marg = name[ ] ~ "," ~ marg[ ];
                            if (marg.length)
                            {
                                char* q = cast(char*)mem.xmalloc(namelen + 1 + marg.length);
                                assert(q);
                                memcpy(q, name, namelen);
                                q[namelen] = ',';
                                memcpy(q + namelen + 1, marg.ptr, marg.length);
                                marg = q[0 .. marg.length + namelen + 1];
                            }
                            else
                            {
                                marg = name[0 .. namelen];
                            }
                        }
                    }
                    if (m)
                    {
                        if (m.inuse && marg.length == 0)
                        {
                            // Remove macro invocation
                            buf.remove(u, v + 1 - u);
                            end -= v + 1 - u;
                        }
                        else if (m.inuse && ((arg.length == marg.length && memcmp(arg.ptr, marg.ptr, arg.length) == 0) ||
                                             (arg.length + 4 == marg.length && marg[0] == 0xFF && marg[1] == '{' && memcmp(arg.ptr, marg.ptr + 2, arg.length) == 0 && marg[marg.length - 2] == 0xFF && marg[marg.length - 1] == '}')))
                        {
                            /* Recursive expansion:
                             *   marg is same as arg (with blue paint added)
                             * Just leave in place.
                             */
                        }
                        else
                        {
                            //printf("\tmacro '%.*s'(%.*s) = '%.*s'\n", cast(int)m.namelen, m.name, cast(int)marg.length, marg.ptr, cast(int)m.textlen, m.text);
                            marg = memdup(marg);
                            // Insert replacement text
                            buf.spread(v + 1, 2 + m.text.length + 2);
                            ubyte[] slice = cast(ubyte[])buf[];
                            slice[v + 1] = 0xFF;
                            slice[v + 2] = '{';
                            slice[v + 3 .. v + 3 + m.text.length] = cast(ubyte[])m.text[];
                            slice[v + 3 + m.text.length] = 0xFF;
                            slice[v + 3 + m.text.length + 1] = '}';
                            end += 2 + m.text.length + 2;
                            // Scan replaced text for further expansion
                            m.inuse++;
                            size_t mend = v + 1 + 2 + m.text.length + 2;
                            const success = expand(buf, v + 1, mend, marg, recursionLimit, isIdStart, isIdTail);
                            if (!success)
                                return false;
                            end += mend - (v + 1 + 2 + m.text.length + 2);
                            m.inuse--;
                            buf.remove(u, v + 1 - u);
                            end -= v + 1 - u;
                            u += mend - (v + 1);
                            mem.xfree(cast(char*)marg.ptr);
                            //printf("u = %d, end = %d\n", u, end);
                            //printf("#%.*s#\n", cast(int)(end - u), &buf.data[u]);
                            continue;
                        }
                    }
                    else
                    {
                        // Replace $(NAME) with nothing
                        buf.remove(u, v + 1 - u);
                        end -= (v + 1 - u);
                        continue;
                    }
                }
            }
            u++;
        }
        mem.xfree(cast(char*)arg);
        pend = end;
        return true;
    }

  private:

    Macro* search(const(char)[] name) @nogc nothrow pure @safe
    {
        //printf("Macro::search(%.*s)\n", cast(int)name.length, name.ptr);
        if (auto table = name in mactab)
        {
            //printf("\tfound %d\n", table.textlen);
            return *table;
        }
        return null;
    }

    private Macro*[const(char)[]] mactab;
}

/* ************************************************************************ */

private:

struct Macro
{
    const(char)[] name;     // macro name
    const(char)[] text;     // macro replacement text
    int inuse;              // macro is in use (don't expand)

    this(const(char)[] name, const(char)[] text) @nogc nothrow pure @safe
    {
        this.name = name;
        this.text = text;
    }
}

/************************
 * Make mutable copy of slice p.
 * Params:
 *      p = slice
 * Returns:
 *      copy allocated with mem.xmalloc()
 */

char[] memdup(const(char)[] p) nothrow pure @trusted
{
    size_t len = p.length;
    return (cast(char*)memcpy(mem.xmalloc(len), p.ptr, len))[0 .. len];
}

/**********************************************************
 * Given buffer buf[], extract argument marg[].
 * Params:
 *      buf = source string
 *      marg = set to slice of buf[]
 *      n =     0:      get entire argument
 *              1..9:   get nth argument
 *              -1:     get 2nd through end
 */
size_t extractArgN(const(char)[] buf, out const(char)[] marg, int n) @nogc nothrow pure @safe
{
    /* Scan forward for matching right parenthesis.
     * Nest parentheses.
     * Skip over "..." and '...' strings inside HTML tags.
     * Skip over <!-- ... --> comments.
     * Skip over previous macro insertions
     * Set marg.
     */
    uint parens = 1;
    ubyte instring = 0;
    uint incomment = 0;
    uint intag = 0;
    uint inexp = 0;
    uint argn = 0;
    size_t v = 0;
    const p = buf;
    const end = buf.length;
Largstart:
    // Skip first space, if any, to find the start of the macro argument
    if (n != 1 && v < end && isspace(p[v]))
        v++;
    size_t vstart = v;
    for (; v < end; v++)
    {
        char c = p[v];
        switch (c)
        {
        case ',':
            if (!inexp && !instring && !incomment && parens == 1)
            {
                argn++;
                if (argn == 1 && n == -1)
                {
                    v++;
                    goto Largstart;
                }
                if (argn == n)
                    break;
                if (argn + 1 == n)
                {
                    v++;
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
                if (v + 6 < end && p[v + 1] == '!' && p[v + 2] == '-' && p[v + 3] == '-')
                {
                    incomment = 1;
                    v += 3;
                }
                else if (v + 2 < end && isalpha(p[v + 1]))
                    intag = 1;
            }
            continue;
        case '>':
            if (!inexp)
                intag = 0;
            continue;
        case '-':
            if (!inexp && !instring && incomment && v + 2 < end && p[v + 1] == '-' && p[v + 2] == '>')
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
        marg = p[v .. v];
    else
        marg = p[vstart .. v];
    //printf("extractArg%d('%.*s') = '%.*s'\n", n, cast(int)end, p, cast(int)marg.length, marg.ptr);
    return v;
}

/*****************************************
 * Get number of UTF-8 code units in code point that starts with `c`
 * Params:
 *      c = starting code unit
 * Returns: number of UTF-8 code units (i.e. bytes), else 1 on invalid UTF start
 */
@safe
int utfStride(char c) @nogc nothrow pure
{
    return
        c < 0x80 ? 1 :
        c < 0xC0 ? 1 : // invalid UTF start
        c < 0xE0 ? 2 :
        c < 0xF0 ? 3 :
        c < 0xF8 ? 4 :
        c < 0xFC ? 5 :
        c < 0xFE ? 6 :
                   1; // invalid UTF start
}

unittest
{
    assert(utfStride(0) == 1);
    assert(utfStride(0x80) == 1);
    assert(utfStride(0xC0) == 2);
    assert(utfStride(0xE0) == 3);
    assert(utfStride(0xF0) == 4);
    assert(utfStride(0xF8) == 5);
    assert(utfStride(0xFC) == 6);
    assert(utfStride(0xFE) == 1);
}
