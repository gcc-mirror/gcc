
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/lexer.c
 */

/* Lexical Analyzer */

#include "root/dsystem.h" // for time() and ctime()
#include "root/rmem.h"

#include "mars.h"
#include "lexer.h"
#include "utf.h"
#include "identifier.h"
#include "id.h"

extern int HtmlNamedEntity(const utf8_t *p, size_t length);

#define LS 0x2028       // UTF line separator
#define PS 0x2029       // UTF paragraph separator

/********************************************
 * Do our own char maps
 */

static unsigned char cmtable[256];

const int CMoctal =     0x1;
const int CMhex =       0x2;
const int CMidchar =    0x4;

inline bool isoctal (utf8_t c) { return (cmtable[c] & CMoctal) != 0; }
inline bool ishex   (utf8_t c) { return (cmtable[c] & CMhex) != 0; }
inline bool isidchar(utf8_t c) { return (cmtable[c] & CMidchar) != 0; }

struct CMTableInitializer
{
    CMTableInitializer();
};

static CMTableInitializer cmtableinitializer;

CMTableInitializer::CMTableInitializer()
{
    for (unsigned c = 0; c < 256; c++)
    {
        if ('0' <= c && c <= '7')
            cmtable[c] |= CMoctal;
        if (isxdigit(c))
            cmtable[c] |= CMhex;
        if (isalnum(c) || c == '_')
            cmtable[c] |= CMidchar;
    }
}

/*************************** Lexer ********************************************/

OutBuffer Lexer::stringbuffer;

Lexer::Lexer(const char *filename,
        const utf8_t *base, size_t begoffset, size_t endoffset,
        bool doDocComment, bool commentToken)
{
    scanloc = Loc(filename, 1, 1);
    //printf("Lexer::Lexer(%p,%d)\n",base,length);
    //printf("lexer.filename = %s\n", filename);
    this->token = Token();
    this->token.ptr = NULL;
    this->token.value = TOKreserved;
    this->token.blockComment = NULL;
    this->token.lineComment = NULL;
    this->base = base;
    this->end  = base + endoffset;
    p = base + begoffset;
    line = p;
    this->doDocComment = doDocComment;
    this->anyToken = 0;
    this->commentToken = commentToken;
    this->errors = false;
    //initKeywords();

    /* If first line starts with '#!', ignore the line
     */

    if (p[0] == '#' && p[1] =='!')
    {
        p += 2;
        while (1)
        {
            utf8_t c = *p++;
            switch (c)
            {
                case 0:
                case 0x1A:
                    p--;
                    /* fall through */

                case '\n':
                    break;

                default:
                    continue;
            }
            break;
        }
        endOfLine();
    }
}


void Lexer::endOfLine()
{
    scanloc.linnum++;
    line = p;
}


void Lexer::error(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    ::verror(token.loc, format, ap);
    va_end(ap);
    errors = true;
}

void Lexer::error(Loc loc, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    ::verror(loc, format, ap);
    va_end(ap);
    errors = true;
}

void Lexer::deprecation(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    ::vdeprecation(token.loc, format, ap);
    va_end(ap);
    if (global.params.useDeprecated == DIAGNOSTICerror)
        errors = true;
}

TOK Lexer::nextToken()
{
    if (token.next)
    {
        Token *t = token.next;
        memcpy(&token,t,sizeof(Token));
        t->free();
    }
    else
    {
        scan(&token);
    }
    //token.print();
    return token.value;
}

Token *Lexer::peek(Token *ct)
{
    Token *t;
    if (ct->next)
        t = ct->next;
    else
    {
        t = Token::alloc();
        scan(t);
        ct->next = t;
    }
    return t;
}

/***********************
 * Look ahead at next token's value.
 */

TOK Lexer::peekNext()
{
    return peek(&token)->value;
}

/***********************
 * Look 2 tokens ahead at value.
 */

TOK Lexer::peekNext2()
{
    Token *t = peek(&token);
    return peek(t)->value;
}

/*********************************
 * tk is on the opening (.
 * Look ahead and return token that is past the closing ).
 */

Token *Lexer::peekPastParen(Token *tk)
{
    //printf("peekPastParen()\n");
    int parens = 1;
    int curlynest = 0;
    while (1)
    {
        tk = peek(tk);
        //tk->print();
        switch (tk->value)
        {
            case TOKlparen:
                parens++;
                continue;

            case TOKrparen:
                --parens;
                if (parens)
                    continue;
                tk = peek(tk);
                break;

            case TOKlcurly:
                curlynest++;
                continue;

            case TOKrcurly:
                if (--curlynest >= 0)
                    continue;
                break;

            case TOKsemicolon:
                if (curlynest)
                    continue;
                break;

            case TOKeof:
                break;

            default:
                continue;
        }
        return tk;
    }
}

/****************************
 * Turn next token in buffer into a token.
 */

void Lexer::scan(Token *t)
{
    unsigned lastLine = scanloc.linnum;
    Loc startLoc;

    t->blockComment = NULL;
    t->lineComment = NULL;
    while (1)
    {
        t->ptr = p;
        //printf("p = %p, *p = '%c'\n",p,*p);
        t->loc = loc();
        switch (*p)
        {
            case 0:
            case 0x1A:
                t->value = TOKeof;                      // end of file
                return;

            case ' ':
            case '\t':
            case '\v':
            case '\f':
                p++;
                continue;                       // skip white space

            case '\r':
                p++;
                if (*p != '\n')                 // if CR stands by itself
                    endOfLine();
                continue;                       // skip white space

            case '\n':
                p++;
                endOfLine();
                continue;                       // skip white space

            case '0':   case '1':   case '2':   case '3':   case '4':
            case '5':   case '6':   case '7':   case '8':   case '9':
                t->value = number(t);
                return;

            case '\'':
                t->value = charConstant(t);
                return;

            case 'r':
                if (p[1] != '"')
                    goto case_ident;
                p++;
                /* fall through */
            case '`':
                t->value = wysiwygStringConstant(t, *p);
                return;

            case 'x':
                if (p[1] != '"')
                    goto case_ident;
                p++;
                t->value = hexStringConstant(t);
                return;

            case 'q':
                if (p[1] == '"')
                {
                    p++;
                    t->value = delimitedStringConstant(t);
                    return;
                }
                else if (p[1] == '{')
                {
                    p++;
                    t->value = tokenStringConstant(t);
                    return;
                }
                else
                    goto case_ident;

            case '"':
                t->value = escapeStringConstant(t);
                return;

            case 'a':   case 'b':   case 'c':   case 'd':   case 'e':
            case 'f':   case 'g':   case 'h':   case 'i':   case 'j':
            case 'k':   case 'l':   case 'm':   case 'n':   case 'o':
            case 'p':   /*case 'q': case 'r':*/ case 's':   case 't':
            case 'u':   case 'v':   case 'w': /*case 'x':*/ case 'y':
            case 'z':
            case 'A':   case 'B':   case 'C':   case 'D':   case 'E':
            case 'F':   case 'G':   case 'H':   case 'I':   case 'J':
            case 'K':   case 'L':   case 'M':   case 'N':   case 'O':
            case 'P':   case 'Q':   case 'R':   case 'S':   case 'T':
            case 'U':   case 'V':   case 'W':   case 'X':   case 'Y':
            case 'Z':
            case '_':
            case_ident:
            {   utf8_t c;

                while (1)
                {
                    c = *++p;
                    if (isidchar(c))
                        continue;
                    else if (c & 0x80)
                    {   const utf8_t *s = p;
                        unsigned u = decodeUTF();
                        if (isUniAlpha(u))
                            continue;
                        error("char 0x%04x not allowed in identifier", u);
                        p = s;
                    }
                    break;
                }

                Identifier *id = Identifier::idPool((const char *)t->ptr, p - t->ptr);
                t->ident = id;
                t->value = (TOK) id->getValue();
                anyToken = 1;
                if (*t->ptr == '_')     // if special identifier token
                {
                    static bool initdone = false;
                    static char date[11+1];
                    static char time[8+1];
                    static char timestamp[24+1];

                    if (!initdone)       // lazy evaluation
                    {
                        initdone = true;
                        time_t ct;
                        ::time(&ct);
                        char *p = ctime(&ct);
                        assert(p);
                        sprintf(&date[0], "%.6s %.4s", p + 4, p + 20);
                        sprintf(&time[0], "%.8s", p + 11);
                        sprintf(&timestamp[0], "%.24s", p);
                    }

                    if (id == Id::DATE)
                    {
                        t->ustring = (utf8_t *)date;
                        goto Lstr;
                    }
                    else if (id == Id::TIME)
                    {
                        t->ustring = (utf8_t *)time;
                        goto Lstr;
                    }
                    else if (id == Id::VENDOR)
                    {
                        t->ustring = (utf8_t *)const_cast<char *>(global.vendor.ptr);
                        goto Lstr;
                    }
                    else if (id == Id::TIMESTAMP)
                    {
                        t->ustring = (utf8_t *)timestamp;
                     Lstr:
                        t->value = TOKstring;
                        t->postfix = 0;
                        t->len = (unsigned)strlen((char *)t->ustring);
                    }
                    else if (id == Id::VERSIONX)
                    {   unsigned major = 0;
                        unsigned minor = 0;
                        bool point = false;

                        for (const char *p = global.version.ptr + 1; 1; p++)
                        {
                            c = *p;
                            if (isdigit((utf8_t)c))
                                minor = minor * 10 + c - '0';
                            else if (c == '.')
                            {
                                if (point)
                                    break;      // ignore everything after second '.'
                                point = true;
                                major = minor;
                                minor = 0;
                            }
                            else
                                break;
                        }
                        t->value = TOKint64v;
                        t->uns64value = major * 1000 + minor;
                    }
                    else if (id == Id::EOFX)
                    {
                        t->value = TOKeof;
                        // Advance scanner to end of file
                        while (!(*p == 0 || *p == 0x1A))
                            p++;
                    }
                }
                //printf("t->value = %d\n",t->value);
                return;
            }

            case '/':
                p++;
                switch (*p)
                {
                    case '=':
                        p++;
                        t->value = TOKdivass;
                        return;

                    case '*':
                        p++;
                        startLoc = loc();
                        while (1)
                        {
                            while (1)
                            {   utf8_t c = *p;
                                switch (c)
                                {
                                    case '/':
                                        break;

                                    case '\n':
                                        endOfLine();
                                        p++;
                                        continue;

                                    case '\r':
                                        p++;
                                        if (*p != '\n')
                                            endOfLine();
                                        continue;

                                    case 0:
                                    case 0x1A:
                                        error("unterminated /* */ comment");
                                        p = end;
                                        t->loc = loc();
                                        t->value = TOKeof;
                                        return;

                                    default:
                                        if (c & 0x80)
                                        {   unsigned u = decodeUTF();
                                            if (u == PS || u == LS)
                                                endOfLine();
                                        }
                                        p++;
                                        continue;
                                }
                                break;
                            }
                            p++;
                            if (p[-2] == '*' && p - 3 != t->ptr)
                                break;
                        }
                        if (commentToken)
                        {
                            t->loc = startLoc;
                            t->value = TOKcomment;
                            return;
                        }
                        else if (doDocComment && t->ptr[2] == '*' && p - 4 != t->ptr)
                        {   // if /** but not /**/
                            getDocComment(t, lastLine == startLoc.linnum);
                        }
                        continue;

                    case '/':           // do // style comments
                        startLoc = loc();
                        while (1)
                        {   utf8_t c = *++p;
                            switch (c)
                            {
                                case '\n':
                                    break;

                                case '\r':
                                    if (p[1] == '\n')
                                        p++;
                                    break;

                                case 0:
                                case 0x1A:
                                    if (commentToken)
                                    {
                                        p = end;
                                        t->loc = startLoc;
                                        t->value = TOKcomment;
                                        return;
                                    }
                                    if (doDocComment && t->ptr[2] == '/')
                                        getDocComment(t, lastLine == startLoc.linnum);
                                    p = end;
                                    t->loc = loc();
                                    t->value = TOKeof;
                                    return;

                                default:
                                    if (c & 0x80)
                                    {   unsigned u = decodeUTF();
                                        if (u == PS || u == LS)
                                            break;
                                    }
                                    continue;
                            }
                            break;
                        }

                        if (commentToken)
                        {
                            p++;
                            endOfLine();
                            t->loc = startLoc;
                            t->value = TOKcomment;
                            return;
                        }
                        if (doDocComment && t->ptr[2] == '/')
                            getDocComment(t, lastLine == startLoc.linnum);

                        p++;
                        endOfLine();
                        continue;

                    case '+':
                    {   int nest;

                        startLoc = loc();
                        p++;
                        nest = 1;
                        while (1)
                        {   utf8_t c = *p;
                            switch (c)
                            {
                                case '/':
                                    p++;
                                    if (*p == '+')
                                    {
                                        p++;
                                        nest++;
                                    }
                                    continue;

                                case '+':
                                    p++;
                                    if (*p == '/')
                                    {
                                        p++;
                                        if (--nest == 0)
                                            break;
                                    }
                                    continue;

                                case '\r':
                                    p++;
                                    if (*p != '\n')
                                        endOfLine();
                                    continue;

                                case '\n':
                                    endOfLine();
                                    p++;
                                    continue;

                                case 0:
                                case 0x1A:
                                    error("unterminated /+ +/ comment");
                                    p = end;
                                    t->loc = loc();
                                    t->value = TOKeof;
                                    return;

                                default:
                                    if (c & 0x80)
                                    {   unsigned u = decodeUTF();
                                        if (u == PS || u == LS)
                                            endOfLine();
                                    }
                                    p++;
                                    continue;
                            }
                            break;
                        }
                        if (commentToken)
                        {
                            t->loc = startLoc;
                            t->value = TOKcomment;
                            return;
                        }
                        if (doDocComment && t->ptr[2] == '+' && p - 4 != t->ptr)
                        {   // if /++ but not /++/
                            getDocComment(t, lastLine == startLoc.linnum);
                        }
                        continue;
                    }
                    default:
                        break;
                }
                t->value = TOKdiv;
                return;

            case '.':
                p++;
                if (isdigit(*p))
                {   /* Note that we don't allow ._1 and ._ as being
                     * valid floating point numbers.
                     */
                    p--;
                    t->value = inreal(t);
                }
                else if (p[0] == '.')
                {
                    if (p[1] == '.')
                    {   p += 2;
                        t->value = TOKdotdotdot;
                    }
                    else
                    {   p++;
                        t->value = TOKslice;
                    }
                }
                else
                    t->value = TOKdot;
                return;

            case '&':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKandass;
                }
                else if (*p == '&')
                {   p++;
                    t->value = TOKandand;
                }
                else
                    t->value = TOKand;
                return;

            case '|':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKorass;
                }
                else if (*p == '|')
                {   p++;
                    t->value = TOKoror;
                }
                else
                    t->value = TOKor;
                return;

            case '-':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKminass;
                }
                else if (*p == '-')
                {   p++;
                    t->value = TOKminusminus;
                }
                else
                    t->value = TOKmin;
                return;

            case '+':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKaddass;
                }
                else if (*p == '+')
                {   p++;
                    t->value = TOKplusplus;
                }
                else
                    t->value = TOKadd;
                return;

            case '<':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKle;                   // <=
                }
                else if (*p == '<')
                {   p++;
                    if (*p == '=')
                    {   p++;
                        t->value = TOKshlass;           // <<=
                    }
                    else
                        t->value = TOKshl;              // <<
                }
                else if (*p == '>')
                {   p++;
                    if (*p == '=')
                    {   p++;
                        t->value = TOKleg;              // <>=
                    }
                    else
                        t->value = TOKlg;               // <>
                }
                else
                    t->value = TOKlt;                   // <
                return;

            case '>':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKge;                   // >=
                }
                else if (*p == '>')
                {   p++;
                    if (*p == '=')
                    {   p++;
                        t->value = TOKshrass;           // >>=
                    }
                    else if (*p == '>')
                    {   p++;
                        if (*p == '=')
                        {   p++;
                            t->value = TOKushrass;      // >>>=
                        }
                        else
                            t->value = TOKushr;         // >>>
                    }
                    else
                        t->value = TOKshr;              // >>
                }
                else
                    t->value = TOKgt;                   // >
                return;

            case '!':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKnotequal;         // !=
                }
                else if (*p == '<')
                {   p++;
                    if (*p == '>')
                    {   p++;
                        if (*p == '=')
                        {   p++;
                            t->value = TOKunord; // !<>=
                        }
                        else
                            t->value = TOKue;   // !<>
                    }
                    else if (*p == '=')
                    {   p++;
                        t->value = TOKug;       // !<=
                    }
                    else
                        t->value = TOKuge;      // !<
                }
                else if (*p == '>')
                {   p++;
                    if (*p == '=')
                    {   p++;
                        t->value = TOKul;       // !>=
                    }
                    else
                        t->value = TOKule;      // !>
                }
                else
                    t->value = TOKnot;          // !
                return;

            case '=':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKequal;            // ==
                }
                else if (*p == '>')
                {   p++;
                    t->value = TOKgoesto;               // =>
                }
                else
                    t->value = TOKassign;               // =
                return;

            case '~':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKcatass;               // ~=
                }
                else
                    t->value = TOKtilde;                // ~
                return;

            case '^':
                p++;
                if (*p == '^')
                {   p++;
                    if (*p == '=')
                    {   p++;
                        t->value = TOKpowass;  // ^^=
                    }
                    else
                        t->value = TOKpow;     // ^^
                }
                else if (*p == '=')
                {   p++;
                    t->value = TOKxorass;    // ^=
                }
                else
                    t->value = TOKxor;       // ^
                return;

            case '(': p++; t->value = TOKlparen; return;
            case ')': p++; t->value = TOKrparen; return;
            case '[': p++; t->value = TOKlbracket; return;
            case ']': p++; t->value = TOKrbracket; return;
            case '{': p++; t->value = TOKlcurly; return;
            case '}': p++; t->value = TOKrcurly; return;
            case '?': p++; t->value = TOKquestion; return;
            case ',': p++; t->value = TOKcomma; return;
            case ';': p++; t->value = TOKsemicolon; return;
            case ':': p++; t->value = TOKcolon; return;
            case '$': p++; t->value = TOKdollar; return;
            case '@': p++; t->value = TOKat; return;

            case '*':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKmulass;
                }
                else
                    t->value = TOKmul;
                return;
            case '%':
                p++;
                if (*p == '=')
                {   p++;
                    t->value = TOKmodass;
                }
                else
                    t->value = TOKmod;
                return;

            case '#':
            {
                p++;
                Token n;
                scan(&n);
                if (n.value == TOKidentifier)
                {
                   if (n.ident == Id::line)
                   {
                       poundLine();
                       continue;
                   }
                   else
                   {
                       const Loc locx = loc();
                       warning(locx, "C preprocessor directive `#%s` is not supported", n.ident->toChars());
                   }
                }
                else if (n.value == TOKif)
                {
                    error("C preprocessor directive `#if` is not supported, use `version` or `static if`");
                }
                t->value = TOKpound;
                return;
            }

            default:
            {   unsigned c = *p;

                if (c & 0x80)
                {   c = decodeUTF();

                    // Check for start of unicode identifier
                    if (isUniAlpha(c))
                        goto case_ident;

                    if (c == PS || c == LS)
                    {
                        endOfLine();
                        p++;
                        continue;
                    }
                }
                if (c < 0x80 && isprint(c))
                    error("character '%c' is not a valid token", c);
                else
                    error("character 0x%02x is not a valid token", c);
                p++;
                continue;
            }
        }
    }
}

/*******************************************
 * Parse escape sequence.
 */

unsigned Lexer::escapeSequence()
{   unsigned c = *p;

    int n;
    int ndigits;

    switch (c)
    {
        case '\'':
        case '"':
        case '?':
        case '\\':
        Lconsume:
                p++;
                break;

        case 'a':       c = 7;          goto Lconsume;
        case 'b':       c = 8;          goto Lconsume;
        case 'f':       c = 12;         goto Lconsume;
        case 'n':       c = 10;         goto Lconsume;
        case 'r':       c = 13;         goto Lconsume;
        case 't':       c = 9;          goto Lconsume;
        case 'v':       c = 11;         goto Lconsume;

        case 'u':
                ndigits = 4;
                goto Lhex;
        case 'U':
                ndigits = 8;
                goto Lhex;
        case 'x':
                ndigits = 2;
        Lhex:
                p++;
                c = *p;
                if (ishex((utf8_t)c))
                {   unsigned v;

                    n = 0;
                    v = 0;
                    while (1)
                    {
                        if (isdigit((utf8_t)c))
                            c -= '0';
                        else if (islower(c))
                            c -= 'a' - 10;
                        else
                            c -= 'A' - 10;
                        v = v * 16 + c;
                        c = *++p;
                        if (++n == ndigits)
                            break;
                        if (!ishex((utf8_t)c))
                        {   error("escape hex sequence has %d hex digits instead of %d", n, ndigits);
                            break;
                        }
                    }
                    if (ndigits != 2 && !utf_isValidDchar(v))
                    {   error("invalid UTF character \\U%08x", v);
                        v = '?';        // recover with valid UTF character
                    }
                    c = v;
                }
                else
                    error("undefined escape hex sequence \\%c",c);
                break;

        case '&':                       // named character entity
                for (const utf8_t *idstart = ++p; 1; p++)
                {
                    switch (*p)
                    {
                        case ';':
                            c = HtmlNamedEntity(idstart, p - idstart);
                            if (c == ~0U)
                            {   error("unnamed character entity &%.*s;", (int)(p - idstart), idstart);
                                c = ' ';
                            }
                            p++;
                            break;

                        default:
                            if (isalpha(*p) ||
                                (p != idstart && isdigit(*p)))
                                continue;
                            error("unterminated named entity &%.*s;", (int)(p - idstart + 1), idstart);
                            break;
                    }
                    break;
                }
                break;

        case 0:
        case 0x1A:                      // end of file
                c = '\\';
                break;

        default:
                if (isoctal((utf8_t)c))
                {   unsigned v;

                    n = 0;
                    v = 0;
                    do
                    {
                        v = v * 8 + (c - '0');
                        c = *++p;
                    } while (++n < 3 && isoctal((utf8_t)c));
                    c = v;
                    if (c > 0xFF)
                        error("escape octal sequence \\%03o is larger than \\377", c);
                }
                else
                    error("undefined escape sequence \\%c",c);
                break;
    }
    return c;
}

/**************************************
 */

TOK Lexer::wysiwygStringConstant(Token *t, int tc)
{
    int c;
    Loc start = loc();

    p++;
    stringbuffer.reset();
    while (1)
    {
        c = *p++;
        switch (c)
        {
            case '\n':
                endOfLine();
                break;

            case '\r':
                if (*p == '\n')
                    continue;   // ignore
                c = '\n';       // treat EndOfLine as \n character
                endOfLine();
                break;

            case 0:
            case 0x1A:
                error("unterminated string constant starting at %s", start.toChars());
                t->ustring = (utf8_t *)const_cast<char *>("");
                t->len = 0;
                t->postfix = 0;
                return TOKstring;

            case '"':
            case '`':
                if (c == tc)
                {
                    t->len = (unsigned)stringbuffer.length();
                    stringbuffer.writeByte(0);
                    t->ustring = (utf8_t *)mem.xmalloc(stringbuffer.length());
                    memcpy(t->ustring, stringbuffer.slice().ptr, stringbuffer.length());
                    stringPostfix(t);
                    return TOKstring;
                }
                break;

            default:
                if (c & 0x80)
                {   p--;
                    unsigned u = decodeUTF();
                    p++;
                    if (u == PS || u == LS)
                        endOfLine();
                    stringbuffer.writeUTF8(u);
                    continue;
                }
                break;
        }
        stringbuffer.writeByte(c);
    }
}

/**************************************
 * Lex hex strings:
 *      x"0A ae 34FE BD"
 */

TOK Lexer::hexStringConstant(Token *t)
{
    unsigned c;
    Loc start = loc();
    unsigned n = 0;
    unsigned v = ~0; // dead assignment, needed to suppress warning

    p++;
    stringbuffer.reset();
    while (1)
    {
        c = *p++;
        switch (c)
        {
            case ' ':
            case '\t':
            case '\v':
            case '\f':
                continue;                       // skip white space

            case '\r':
                if (*p == '\n')
                    continue;                   // ignore
                // Treat isolated '\r' as if it were a '\n'
                /* fall through */
            case '\n':
                endOfLine();
                continue;

            case 0:
            case 0x1A:
                error("unterminated string constant starting at %s", start.toChars());
                t->ustring = (utf8_t *)const_cast<char *>("");
                t->len = 0;
                t->postfix = 0;
                return TOKxstring;

            case '"':
                if (n & 1)
                {   error("odd number (%d) of hex characters in hex string", n);
                    stringbuffer.writeByte(v);
                }
                t->len = (unsigned)stringbuffer.length();
                stringbuffer.writeByte(0);
                t->ustring = (utf8_t *)mem.xmalloc(stringbuffer.length());
                memcpy(t->ustring, stringbuffer.slice().ptr, stringbuffer.length());
                stringPostfix(t);
                return TOKxstring;

            default:
                if (c >= '0' && c <= '9')
                    c -= '0';
                else if (c >= 'a' && c <= 'f')
                    c -= 'a' - 10;
                else if (c >= 'A' && c <= 'F')
                    c -= 'A' - 10;
                else if (c & 0x80)
                {   p--;
                    unsigned u = decodeUTF();
                    p++;
                    if (u == PS || u == LS)
                        endOfLine();
                    else
                        error("non-hex character \\u%04x in hex string", u);
                }
                else
                    error("non-hex character '%c' in hex string", c);
                if (n & 1)
                {   v = (v << 4) | c;
                    stringbuffer.writeByte(v);
                }
                else
                    v = c;
                n++;
                break;
        }
    }
}


/**************************************
 * Lex delimited strings:
 *      q"(foo(xxx))"   // "foo(xxx)"
 *      q"[foo(]"       // "foo("
 *      q"/foo]/"       // "foo]"
 *      q"HERE
 *      foo
 *      HERE"           // "foo\n"
 * Input:
 *      p is on the "
 */

TOK Lexer::delimitedStringConstant(Token *t)
{
    unsigned c;
    Loc start = loc();
    unsigned delimleft = 0;
    unsigned delimright = 0;
    unsigned nest = 1;
    unsigned nestcount = ~0; // dead assignment, needed to suppress warning
    Identifier *hereid = NULL;
    unsigned blankrol = 0;
    unsigned startline = 0;

    p++;
    stringbuffer.reset();
    while (1)
    {
        c = *p++;
        //printf("c = '%c'\n", c);
        switch (c)
        {
            case '\n':
            Lnextline:
                endOfLine();
                startline = 1;
                if (blankrol)
                {   blankrol = 0;
                    continue;
                }
                if (hereid)
                {
                    stringbuffer.writeUTF8(c);
                    continue;
                }
                break;

            case '\r':
                if (*p == '\n')
                    continue;   // ignore
                c = '\n';       // treat EndOfLine as \n character
                goto Lnextline;

            case 0:
            case 0x1A:
                error("unterminated delimited string constant starting at %s", start.toChars());
                t->ustring = (utf8_t *)const_cast<char *>("");
                t->len = 0;
                t->postfix = 0;
                return TOKstring;

            default:
                if (c & 0x80)
                {   p--;
                    c = decodeUTF();
                    p++;
                    if (c == PS || c == LS)
                        goto Lnextline;
                }
                break;
        }
        if (delimleft == 0)
        {   delimleft = c;
            nest = 1;
            nestcount = 1;
            if (c == '(')
                delimright = ')';
            else if (c == '{')
                delimright = '}';
            else if (c == '[')
                delimright = ']';
            else if (c == '<')
                delimright = '>';
            else if (isalpha(c) || c == '_' || (c >= 0x80 && isUniAlpha(c)))
            {   // Start of identifier; must be a heredoc
                Token tok;
                p--;
                scan(&tok);               // read in heredoc identifier
                if (tok.value != TOKidentifier)
                {   error("identifier expected for heredoc, not %s", tok.toChars());
                    delimright = c;
                }
                else
                {   hereid = tok.ident;
                    //printf("hereid = '%s'\n", hereid->toChars());
                    blankrol = 1;
                }
                nest = 0;
            }
            else
            {   delimright = c;
                nest = 0;
                if (isspace(c))
                    error("delimiter cannot be whitespace");
            }
        }
        else
        {
            if (blankrol)
            {   error("heredoc rest of line should be blank");
                blankrol = 0;
                continue;
            }
            if (nest == 1)
            {
                if (c == delimleft)
                    nestcount++;
                else if (c == delimright)
                {   nestcount--;
                    if (nestcount == 0)
                        goto Ldone;
                }
            }
            else if (c == delimright)
                goto Ldone;
            if (startline && isalpha(c) && hereid)
            {   Token tok;
                const utf8_t *psave = p;
                p--;
                scan(&tok);               // read in possible heredoc identifier
                //printf("endid = '%s'\n", tok.ident->toChars());
                if (tok.value == TOKidentifier && tok.ident->equals(hereid))
                {   /* should check that rest of line is blank
                     */
                    goto Ldone;
                }
                p = psave;
            }
            stringbuffer.writeUTF8(c);
            startline = 0;
        }
    }

Ldone:
    if (*p == '"')
        p++;
    else if (hereid)
        error("delimited string must end in %s\"", hereid->toChars());
    else
        error("delimited string must end in %c\"", delimright);
    t->len = (unsigned)stringbuffer.length();
    stringbuffer.writeByte(0);
    t->ustring = (utf8_t *)mem.xmalloc(stringbuffer.length());
    memcpy(t->ustring, stringbuffer.slice().ptr, stringbuffer.length());
    stringPostfix(t);
    return TOKstring;
}

/**************************************
 * Lex delimited strings:
 *      q{ foo(xxx) } // " foo(xxx) "
 *      q{foo(}       // "foo("
 *      q{{foo}"}"}   // "{foo}"}""
 * Input:
 *      p is on the q
 */

TOK Lexer::tokenStringConstant(Token *t)
{
    unsigned nest = 1;
    Loc start = loc();
    const utf8_t *pstart = ++p;

    while (1)
    {   Token tok;

        scan(&tok);
        switch (tok.value)
        {
            case TOKlcurly:
                nest++;
                continue;

            case TOKrcurly:
                if (--nest == 0)
                {
                    t->len = (unsigned)(p - 1 - pstart);
                    t->ustring = (utf8_t *)mem.xmalloc(t->len + 1);
                    memcpy(t->ustring, pstart, t->len);
                    t->ustring[t->len] = 0;
                    stringPostfix(t);
                    return TOKstring;
                }
                continue;

            case TOKeof:
                error("unterminated token string constant starting at %s", start.toChars());
                t->ustring = (utf8_t *)const_cast<char *>("");
                t->len = 0;
                t->postfix = 0;
                return TOKstring;

            default:
                continue;
        }
    }
}



/**************************************
 */

TOK Lexer::escapeStringConstant(Token *t)
{
    unsigned c;
    Loc start = loc();

    p++;
    stringbuffer.reset();
    while (1)
    {
        c = *p++;
        switch (c)
        {
            case '\\':
                switch (*p)
                {
                    case 'u':
                    case 'U':
                    case '&':
                        c = escapeSequence();
                        stringbuffer.writeUTF8(c);
                        continue;

                    default:
                        c = escapeSequence();
                        break;
                }
                break;
            case '\n':
                endOfLine();
                break;

            case '\r':
                if (*p == '\n')
                    continue;   // ignore
                c = '\n';       // treat EndOfLine as \n character
                endOfLine();
                break;

            case '"':
                t->len = (unsigned)stringbuffer.length();
                stringbuffer.writeByte(0);
                t->ustring = (utf8_t *)mem.xmalloc(stringbuffer.length());
                memcpy(t->ustring, stringbuffer.slice().ptr, stringbuffer.length());
                stringPostfix(t);
                return TOKstring;

            case 0:
            case 0x1A:
                p--;
                error("unterminated string constant starting at %s", start.toChars());
                t->ustring = (utf8_t *)const_cast<char *>("");
                t->len = 0;
                t->postfix = 0;
                return TOKstring;

            default:
                if (c & 0x80)
                {
                    p--;
                    c = decodeUTF();
                    if (c == LS || c == PS)
                    {   c = '\n';
                        endOfLine();
                    }
                    p++;
                    stringbuffer.writeUTF8(c);
                    continue;
                }
                break;
        }
        stringbuffer.writeByte(c);
    }
}

/**************************************
 */

TOK Lexer::charConstant(Token *t)
{
    unsigned c;
    TOK tk = TOKcharv;

    //printf("Lexer::charConstant\n");
    p++;
    c = *p++;
    switch (c)
    {
        case '\\':
            switch (*p)
            {
                case 'u':
                    t->uns64value = escapeSequence();
                    tk = TOKwcharv;
                    break;

                case 'U':
                case '&':
                    t->uns64value = escapeSequence();
                    tk = TOKdcharv;
                    break;

                default:
                    t->uns64value = escapeSequence();
                    break;
            }
            break;
        case '\n':
        L1:
            endOfLine();
            /* fall through */
        case '\r':
        case 0:
        case 0x1A:
        case '\'':
            error("unterminated character constant");
            t->uns64value = '?';
            return tk;

        default:
            if (c & 0x80)
            {
                p--;
                c = decodeUTF();
                p++;
                if (c == LS || c == PS)
                    goto L1;
                if (c < 0xD800 || (c >= 0xE000 && c < 0xFFFE))
                    tk = TOKwcharv;
                else
                    tk = TOKdcharv;
            }
            t->uns64value = c;
            break;
    }

    if (*p != '\'')
    {
        error("unterminated character constant");
        t->uns64value = '?';
        return tk;
    }
    p++;
    return tk;
}

/***************************************
 * Get postfix of string literal.
 */

void Lexer::stringPostfix(Token *t)
{
    switch (*p)
    {
        case 'c':
        case 'w':
        case 'd':
            t->postfix = *p;
            p++;
            break;

        default:
            t->postfix = 0;
            break;
    }
}

/**************************************
 * Read in a number.
 * If it's an integer, store it in tok.TKutok.Vlong.
 *      integers can be decimal, octal or hex
 *      Handle the suffixes U, UL, LU, L, etc.
 * If it's double, store it in tok.TKutok.Vdouble.
 * Returns:
 *      TKnum
 *      TKdouble,...
 */

TOK Lexer::number(Token *t)
{
    int base = 10;
    const utf8_t *start = p;
    unsigned c;
    uinteger_t n = 0;                       // unsigned >=64 bit integer type
    int d;
    bool err = false;
    bool overflow = false;

    c = *p;
    if (c == '0')
    {
        ++p;
        c = *p;
        switch (c)
        {
            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
                n = c - '0';
                ++p;
                base = 8;
                break;

            case 'x':
            case 'X':
                ++p;
                base = 16;
                break;

            case 'b':
            case 'B':
                ++p;
                base = 2;
                break;

            case '.':
                if (p[1] == '.')
                    goto Ldone; // if ".."
                if (isalpha(p[1]) || p[1] == '_' || p[1] & 0x80)
                    goto Ldone; // if ".identifier" or ".unicode"
                goto Lreal; // '.' is part of current token

            case 'i':
            case 'f':
            case 'F':
                goto Lreal;

            case '_':
                ++p;
                base = 8;
                break;

            case 'L':
                if (p[1] == 'i')
                    goto Lreal;
                break;

            default:
                break;
        }
    }

    while (1)
    {
        c = *p;
        switch (c)
        {
            case '0': case '1':
                ++p;
                d = c - '0';
                break;

            case '2': case '3':
            case '4': case '5': case '6': case '7':
                if (base == 2 && !err)
                {
                    error("binary digit expected");
                    err = true;
                }
                ++p;
                d = c - '0';
                break;

            case '8': case '9':
                ++p;
                if (base < 10 && !err)
                {
                    error("radix %d digit expected, not `%c`", base, c);
                    err = true;
                }
                d = c - '0';
                break;

            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
                ++p;
                if (base != 16)
                {
                    if (c == 'e' || c == 'E' || c == 'f' || c == 'F')
                        goto Lreal;
                    if (!err)
                    {
                        error("radix %d digit expected, not `%c`", base, c);
                        err = true;
                    }
                }
                if (c >= 'a')
                    d = c + 10 - 'a';
                else
                    d = c + 10 - 'A';
                break;

            case 'L':
                if (p[1] == 'i')
                    goto Lreal;
                goto Ldone;

            case '.':
                if (p[1] == '.')
                    goto Ldone; // if ".."
                if (base == 10 && (isalpha(p[1]) || p[1] == '_' || p[1] & 0x80))
                    goto Ldone; // if ".identifier" or ".unicode"
                goto Lreal; // otherwise as part of a floating point literal

            case 'p':
            case 'P':
            case 'i':
            Lreal:
                p = start;
                return inreal(t);

            case '_':
                ++p;
                continue;

            default:
                goto Ldone;
        }

        uinteger_t n2 = n * base;
        if ((n2 / base != n || n2 + d < n))
        {
            overflow = true;
        }
        n = n2 + d;

        // if n needs more than 64 bits
        if (sizeof(n) > 8 &&
            n > 0xFFFFFFFFFFFFFFFFULL)
        {
            overflow = true;
        }
    }

Ldone:

    if (overflow && !err)
    {
        error("integer overflow");
        err = true;
    }

    enum FLAGS
    {
        FLAGS_none     = 0,
        FLAGS_decimal  = 1,             // decimal
        FLAGS_unsigned = 2,             // u or U suffix
        FLAGS_long     = 4,             // L suffix
    };

    unsigned flags = (base == 10) ? FLAGS_decimal : FLAGS_none;

    // Parse trailing 'u', 'U', 'l' or 'L' in any combination
    const utf8_t *psuffix = p;
    while (1)
    {
        utf8_t f;
        switch (*p)
        {
            case 'U':
            case 'u':
                f = FLAGS_unsigned;
                goto L1;

            case 'l':
                f = FLAGS_long;
                error("lower case integer suffix 'l' is not allowed. Please use 'L' instead");
                goto L1;

            case 'L':
                f = FLAGS_long;
            L1:
                p++;
                if ((flags & f) && !err)
                {
                    error("unrecognized token");
                    err = true;
                }
                flags = (FLAGS) (flags | f);
                continue;
            default:
                break;
        }
        break;
    }

    if (base == 8 && n >= 8)
        error("octal literals 0%llo%.*s are no longer supported, use std.conv.octal!%llo%.*s instead",
                n, p - psuffix, psuffix, n, p - psuffix, psuffix);

    TOK result;
    switch (flags)
    {
        case FLAGS_none:
            /* Octal or Hexadecimal constant.
             * First that fits: int, uint, long, ulong
             */
            if (n & 0x8000000000000000LL)
                result = TOKuns64v;
            else if (n & 0xFFFFFFFF00000000LL)
                result = TOKint64v;
            else if (n & 0x80000000)
                result = TOKuns32v;
            else
                result = TOKint32v;
            break;

        case FLAGS_decimal:
            /* First that fits: int, long, long long
             */
            if (n & 0x8000000000000000LL)
            {
                if (!err)
                {
                    error("signed integer overflow");
                    err = true;
                }
                result = TOKuns64v;
            }
            else if (n & 0xFFFFFFFF80000000LL)
                result = TOKint64v;
            else
                result = TOKint32v;
            break;

        case FLAGS_unsigned:
        case FLAGS_decimal | FLAGS_unsigned:
            /* First that fits: uint, ulong
             */
            if (n & 0xFFFFFFFF00000000LL)
                result = TOKuns64v;
            else
                result = TOKuns32v;
            break;

        case FLAGS_decimal | FLAGS_long:
            if (n & 0x8000000000000000LL)
            {
                if (!err)
                {
                    error("signed integer overflow");
                    err = true;
                }
                result = TOKuns64v;
            }
            else
                result = TOKint64v;
            break;

        case FLAGS_long:
            if (n & 0x8000000000000000LL)
                result = TOKuns64v;
            else
                result = TOKint64v;
            break;

        case FLAGS_unsigned | FLAGS_long:
        case FLAGS_decimal | FLAGS_unsigned | FLAGS_long:
            result = TOKuns64v;
            break;

        default:
            assert(0);
    }
    t->uns64value = n;
    return result;
}

/**************************************
 * Read in characters, converting them to real.
 * Bugs:
 *      Exponent overflow not detected.
 *      Too much requested precision is not detected.
 */

TOK Lexer::inreal(Token *t)
{
    //printf("Lexer::inreal()\n");
    bool isWellformedString = true;
    stringbuffer.reset();
    const utf8_t *pstart = p;
    char hex = 0;
    unsigned c = *p++;

    // Leading '0x'
    if (c == '0')
    {
        c = *p++;
        if (c == 'x' || c == 'X')
        {
            hex = true;
            c = *p++;
        }
    }

    // Digits to left of '.'
    while (1)
    {
        if (c == '.')
        {
            c = *p++;
            break;
        }
        if (isdigit(c) || (hex && isxdigit(c)) || c == '_')
        {
            c = *p++;
            continue;
        }
        break;
    }

    // Digits to right of '.'
    while (1)
    {
        if (isdigit(c) || (hex && isxdigit(c)) || c == '_')
        {
            c = *p++;
            continue;
        }
        break;
    }

    if (c == 'e' || c == 'E' || (hex && (c == 'p' || c == 'P')))
    {
        c = *p++;
        if (c == '-' || c == '+')
        {
            c = *p++;
        }
        bool anyexp = false;
        while (1)
        {
            if (isdigit(c))
            {
                anyexp = true;
                c = *p++;
                continue;
            }
            if (c == '_')
            {
                c = *p++;
                continue;
            }
            if (!anyexp)
            {
                error("missing exponent");
                isWellformedString = false;
            }
            break;
        }
    }
    else if (hex)
    {
        error("exponent required for hex float");
        isWellformedString = false;
    }
    --p;
    while (pstart < p)
    {
        if (*pstart != '_')
            stringbuffer.writeByte(*pstart);
        ++pstart;
    }

    stringbuffer.writeByte(0);
    const char *sbufptr = (char *)stringbuffer.slice().ptr;
    TOK result;
    bool isOutOfRange = false;
    t->floatvalue = (isWellformedString ? CTFloat::parse(sbufptr, &isOutOfRange) : CTFloat::zero);
    errno = 0;
    switch (*p)
    {
        case 'F':
        case 'f':
            if (isWellformedString && !isOutOfRange)
                isOutOfRange = Port::isFloat32LiteralOutOfRange(sbufptr);
            result = TOKfloat32v;
            p++;
            break;

        default:
            if (isWellformedString && !isOutOfRange)
                isOutOfRange = Port::isFloat64LiteralOutOfRange(sbufptr);
            result = TOKfloat64v;
            break;

        case 'l':
            error("use 'L' suffix instead of 'l'");
            /* fall through */
        case 'L':
            result = TOKfloat80v;
            p++;
            break;
    }
    if (*p == 'i' || *p == 'I')
    {
        if (*p == 'I')
            error("use 'i' suffix instead of 'I'");
        p++;
        switch (result)
        {
            case TOKfloat32v:
                result = TOKimaginary32v;
                break;
            case TOKfloat64v:
                result = TOKimaginary64v;
                break;
            case TOKfloat80v:
                result = TOKimaginary80v;
                break;
            default: break;
        }
    }
    const bool isLong = (result == TOKfloat80v || result == TOKimaginary80v);
    if (isOutOfRange && !isLong)
    {
        const char *suffix = (result == TOKfloat32v || result == TOKimaginary32v) ? "f" : "";
        error(scanloc, "number `%s%s` is not representable", (char *)stringbuffer.slice().ptr, suffix);
    }
    return result;
}

/*********************************************
 * parse:
 *      #line linnum [filespec]
 * also allow __LINE__ for linnum, and __FILE__ for filespec
 */

void Lexer::poundLine()
{
    Token tok;
    int linnum = this->scanloc.linnum;
    char *filespec = NULL;
    Loc loc = this->loc();

    scan(&tok);
    if (tok.value == TOKint32v || tok.value == TOKint64v)
    {
        int lin = (int)(tok.uns64value - 1);
        if ((unsigned)lin != tok.uns64value - 1)
            error("line number %lld out of range", (unsigned long long)tok.uns64value);
        else
            linnum = lin;
    }
    else if (tok.value == TOKline)
    {
    }
    else
        goto Lerr;

    while (1)
    {
        switch (*p)
        {
            case 0:
            case 0x1A:
            case '\n':
            Lnewline:
                this->scanloc.linnum = linnum;
                if (filespec)
                    this->scanloc.filename = filespec;
                return;

            case '\r':
                p++;
                if (*p != '\n')
                {   p--;
                    goto Lnewline;
                }
                continue;

            case ' ':
            case '\t':
            case '\v':
            case '\f':
                p++;
                continue;                       // skip white space

            case '_':
                if (memcmp(p, "__FILE__", 8) == 0)
                {
                    p += 8;
                    filespec = mem.xstrdup(scanloc.filename);
                    continue;
                }
                goto Lerr;

            case '"':
                if (filespec)
                    goto Lerr;
                stringbuffer.reset();
                p++;
                while (1)
                {   unsigned c;

                    c = *p;
                    switch (c)
                    {
                        case '\n':
                        case '\r':
                        case 0:
                        case 0x1A:
                            goto Lerr;

                        case '"':
                            stringbuffer.writeByte(0);
                            filespec = mem.xstrdup((char *)stringbuffer.slice().ptr);
                            p++;
                            break;

                        default:
                            if (c & 0x80)
                            {   unsigned u = decodeUTF();
                                if (u == PS || u == LS)
                                    goto Lerr;
                            }
                            stringbuffer.writeByte(c);
                            p++;
                            continue;
                    }
                    break;
                }
                continue;

            default:
                if (*p & 0x80)
                {   unsigned u = decodeUTF();
                    if (u == PS || u == LS)
                        goto Lnewline;
                }
                goto Lerr;
        }
    }

Lerr:
    error(loc, "#line integer [\"filespec\"]\\n expected");
}


/********************************************
 * Decode UTF character.
 * Issue error messages for invalid sequences.
 * Return decoded character, advance p to last character in UTF sequence.
 */

unsigned Lexer::decodeUTF()
{
    dchar_t u;
    utf8_t c;
    const utf8_t *s = p;
    size_t len;
    size_t idx;
    const char *msg;

    c = *s;
    assert(c & 0x80);

    // Check length of remaining string up to 6 UTF-8 characters
    for (len = 1; len < 6 && s[len]; len++)
        ;

    idx = 0;
    msg = utf_decodeChar(s, len, &idx, &u);
    p += idx - 1;
    if (msg)
    {
        error("%s", msg);
    }
    return u;
}

static void trimTrailingWhitespace(OutBuffer &buf)
{
    const unsigned char *s = buf.slice().ptr;
    size_t len = buf.length();
    while (len && (s[len - 1] == ' ' || s[len - 1] == '\t'))
        --len;
    buf.setsize(len);
}

/***************************************************
 * Parse doc comment embedded between t->ptr and p.
 * Remove trailing blanks and tabs from lines.
 * Replace all newlines with \n.
 * Remove leading comment character from each line.
 * Decide if it's a lineComment or a blockComment.
 * Append to previous one for this token.
 */

void Lexer::getDocComment(Token *t, unsigned lineComment)
{
    /* ct tells us which kind of comment it is: '/', '*', or '+'
     */
    utf8_t ct = t->ptr[2];

    /* Start of comment text skips over / * *, / + +, or / / /
     */
    const utf8_t *q = t->ptr + 3;      // start of comment text

    const utf8_t *qend = p;
    if (ct == '*' || ct == '+')
        qend -= 2;

    /* Scan over initial row of ****'s or ++++'s or ////'s
     */
    for (; q < qend; q++)
    {
        if (*q != ct)
            break;
    }

    /* Remove leading spaces until start of the comment
     */
    int linestart = 0;
    if (ct == '/')
    {
        while (q < qend && (*q == ' ' || *q == '\t'))
            ++q;
    }
    else if (q < qend)
    {
        if (*q == '\r')
        {
            ++q;
            if (q < qend && *q == '\n')
                ++q;
            linestart = 1;
        }
        else if (*q == '\n')
        {
            ++q;
            linestart = 1;
        }
    }

    /* Remove trailing row of ****'s or ++++'s
     */
    if (ct != '/')
    {
        for (; q < qend; qend--)
        {
            if (qend[-1] != ct)
                break;
        }
    }

    /* Comment is now [q .. qend].
     * Canonicalize it into buf[].
     */
    OutBuffer buf;

    for (; q < qend; q++)
    {
        utf8_t c = *q;

        switch (c)
        {
            case '*':
            case '+':
                if (linestart && c == ct)
                {   linestart = 0;
                    /* Trim preceding whitespace up to preceding \n
                     */
                    trimTrailingWhitespace(buf);
                    continue;
                }
                break;

            case ' ':
            case '\t':
                break;

            case '\r':
                if (q[1] == '\n')
                    continue;           // skip the \r
                goto Lnewline;

            default:
                if (c == 226)
                {
                    // If LS or PS
                    if (q[1] == 128 &&
                        (q[2] == 168 || q[2] == 169))
                    {
                        q += 2;
                        goto Lnewline;
                    }
                }
                linestart = 0;
                break;

            Lnewline:
                c = '\n';               // replace all newlines with \n
                /* fall through */
            case '\n':
                linestart = 1;

                /* Trim trailing whitespace
                 */
                trimTrailingWhitespace(buf);
                break;
        }
        buf.writeByte(c);
    }

    /* Trim trailing whitespace (if the last line does not have newline)
     */
    if (buf.length() && (buf.slice().ptr[buf.length() - 1] == ' ' || buf.slice().ptr[buf.length() - 1] == '\t'))
    {
        trimTrailingWhitespace(buf);
    }

    // Always end with a newline
    if (!buf.length() || buf.slice().ptr[buf.length() - 1] != '\n')
        buf.writeByte('\n');

    buf.writeByte(0);

    // It's a line comment if the start of the doc comment comes
    // after other non-whitespace on the same line.
    const utf8_t** dc = (lineComment && anyToken)
                         ? &t->lineComment
                         : &t->blockComment;

    // Combine with previous doc comment, if any
    if (*dc)
        *dc = combineComments(*dc, (utf8_t *)buf.slice().ptr);
    else
        *dc = (utf8_t *)buf.extractData();
}

/********************************************
 * Combine two document comments into one,
 * separated by a newline.
 */

const utf8_t *Lexer::combineComments(const utf8_t *c1, const utf8_t *c2)
{
    //printf("Lexer::combineComments('%s', '%s')\n", c1, c2);

    const utf8_t *c = c2;

    if (c1)
    {
        c = c1;
        if (c2)
        {
            size_t len1 = strlen((const char *)c1);
            size_t len2 = strlen((const char *)c2);

            int insertNewLine = 0;
            if (len1 && c1[len1 - 1] != '\n')
            {
                ++len1;
                insertNewLine = 1;
            }

            utf8_t *p = (utf8_t *)mem.xmalloc(len1 + 1 + len2 + 1);
            memcpy(p, c1, len1 - insertNewLine);
            if (insertNewLine)
                p[len1 - 1] = '\n';

            p[len1] = '\n';

            memcpy(p + len1 + 1, c2, len2);
            p[len1 + 1 + len2] = 0;
            c = p;
        }
    }
    return c;
}
