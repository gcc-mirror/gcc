/**
 * Implements the lexical analyzer, which converts source code into lexical tokens.
 *
 * Specification: $(LINK2 https://dlang.org/spec/lex.html, Lexical)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/lexer.d, _lexer.d)
 * Documentation:  https://dlang.org/phobos/dmd_lexer.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/lexer.d
 */

module dmd.lexer;

import core.stdc.ctype;
import core.stdc.stdio;
import core.stdc.string;

import dmd.entity;
import dmd.errorsink;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.common.smallbuffer;
import dmd.common.outbuffer;
import dmd.common.charactertables;
import dmd.root.array;
import dmd.root.ctfloat;
import dmd.root.port;
import dmd.root.rmem;
import dmd.root.utf;
import dmd.tokens;

nothrow:

/***********************************************************
 * Values to use for various magic identifiers
 */
struct CompileEnv
{
    import dmd.common.charactertables;

    uint versionNumber;      /// __VERSION__
    const(char)[] date;      /// __DATE__
    const(char)[] time;      /// __TIME__
    const(char)[] vendor;    /// __VENDOR__
    const(char)[] timestamp; /// __TIMESTAMP__

    bool previewIn;          /// `in` means `[ref] scope const`, accepts rvalues
    bool transitionIn;       /// `-transition=in` is active, `in` parameters are listed
    bool ddocOutput;         /// collect embedded documentation comments
    bool masm;               /// use MASM inline asm syntax

    // these need a default otherwise tests won't work.
    IdentifierCharLookup cCharLookupTable; /// C identifier table (set to the lexer by the C parser)
    IdentifierCharLookup dCharLookupTable; /// D identifier table
}

/***********************************************************
 */
class Lexer
{
    private __gshared OutBuffer stringbuffer;

    BaseLoc* baseLoc;       // Used to generate `scanloc`, which is just an index into this data structure
    Loc scanloc;            // for error messages
    Loc prevloc;            // location of token before current
    int linnum;             // current line number

    const(char)* p;         // current character

    Token token;

    IdentifierCharLookup charLookup; /// Character table for identifiers

    // For ImportC
    bool Ccompile;              /// true if compiling ImportC

    // The following are valid only if (Ccompile == true)
    ubyte boolsize;             /// size of a C _Bool, default 1
    ubyte shortsize;            /// size of a C short, default 2
    ubyte intsize;              /// size of a C int, default 4
    ubyte longsize;             /// size of C long, 4 or 8
    ubyte long_longsize;        /// size of a C long long, default 8
    ubyte long_doublesize;      /// size of C long double, 8 or D real.sizeof
    ubyte wchar_tsize;          /// size of C wchar_t, 2 or 4

    ErrorSink eSink;            /// send error messages through this interface
    CompileEnv compileEnv;      /// environment

    private
    {
        const(char)* base;      // pointer to start of buffer
        const(char)* end;       // pointer to last element of buffer
        const(char)* line;      // start of current line

        bool doDocComment;      // collect doc comment information
        bool anyToken;          // seen at least one token
        bool commentToken;      // comments are TOK.comment's
        bool tokenizeNewlines;  // newlines are turned into TOK.endOfLine's

        bool whitespaceToken;   // tokenize whitespaces (only for DMDLIB)

        int inTokenStringConstant; // can be larger than 1 when in nested q{} strings
        int lastDocLine;        // last line of previous doc comment

        Token* tokenFreelist;
    }

  nothrow:

    /*********************
     * Creates a Lexer for the source code base[begoffset..endoffset+1].
     * The last character, base[endoffset], must be null (0) or EOF (0x1A).
     *
     * Params:
     *  filename = used for error messages
     *  base = source code, must be terminated by a null (0) or EOF (0x1A) character
     *  begoffset = starting offset into base[]
     *  endoffset = the last offset to read into base[]
     *  doDocComment = handle documentation comments
     *  commentToken = comments become TOK.comment's
     *  errorSink = where error messages go, must not be null
     *  compileEnv = version, vendor, date, time, etc.
     */
    this(const(char)* filename, const(char)* base, size_t begoffset,
        size_t endoffset, bool doDocComment, bool commentToken,
        ErrorSink errorSink,
        const CompileEnv* compileEnv) scope
    {
        // debug printf("Lexer::Lexer(%p)\n", base);
        // debug printf("lexer.filename = %s\n", filename);
        token = Token.init;
        this.baseLoc = newBaseLoc(filename, base[0 .. endoffset]);
        this.linnum = 1;
        this.base = base;
        this.end = base + endoffset;
        p = base + begoffset;
        line = p;
        this.doDocComment = doDocComment;
        this.commentToken = commentToken;
        this.tokenizeNewlines = false;
        this.inTokenStringConstant = 0;
        this.lastDocLine = 0;
        this.eSink = errorSink;
        assert(errorSink);
        if (compileEnv)
            this.compileEnv = *compileEnv;
        else
        {
            this.compileEnv.versionNumber = 1;
            this.compileEnv.vendor = "DLF";
            this.compileEnv.cCharLookupTable = IdentifierCharLookup.forTable(IdentifierTable.LR);
            this.compileEnv.dCharLookupTable = IdentifierCharLookup.forTable(IdentifierTable.LR);
        }
        //initKeywords();
        /* If first line starts with '#!', ignore the line
         */
        if (p && p[0] == '#' && p[1] == '!')
        {
            p += 2;
            for (;;p++)
            {
                char c = *p;
                switch (c)
                {
                case '\n':
                    p++;
                    goto case;
                case 0:
                case 0x1A:
                    break;

                default:
                    // Note: We do allow malformed UTF-8 on shebang line.
                    // It could have a meaning if the native system
                    // encoding is not Unicode. See test compilable/test13512.d
                    // for example encoded in KOI-8.
                    // We also allow bidirectional control characters.
                    // We do not execute the shebang line, so it can't be used
                    // to conceal code. It is up to the shell to sanitize it.
                    continue;
                }
                break;
            }
            endOfLine();
        }

        // setup the identifier table lookup functions
        // C tables are setup in its parser constructor
        // Due to us not knowing if we're in C at this point in time.
        charLookup = this.compileEnv.dCharLookupTable;
    }

    /***********************
     * Alternative entry point for DMDLIB, adds `whitespaceToken`
     */
    this(const(char)* filename, const(char)* base, size_t begoffset, size_t endoffset,
        bool doDocComment, bool commentToken, bool whitespaceToken,
        ErrorSink errorSink, const CompileEnv* compileEnv = null
        )
    {
        this(filename, base, begoffset, endoffset, doDocComment, commentToken, errorSink, compileEnv);
        this.whitespaceToken = whitespaceToken;
    }

    /******************
     * Used for unittests for a mock Lexer
     */
    this(ErrorSink errorSink) scope @safe { assert(errorSink); this.eSink = errorSink; }

    /**************************************
     * Reset lexer to lex #define's
     */
    final void resetDefineLines(const(char)[] slice)
    {
        base = slice.ptr;
        end = base + slice.length;
        assert(*end == 0);
        p = base;
        line = p;
        tokenizeNewlines = true;
        inTokenStringConstant = 0;
        lastDocLine = 0;

        baseLoc = newBaseLoc("#defines", slice);
        scanloc = baseLoc.getLoc(0);
    }

    /**********************************
     * Set up for next #define line.
     * p should be at start of next line.
     */
    final void nextDefineLine()
    {
        tokenizeNewlines = true;
    }

    /***************
     * Range interface
     */

    final bool empty() const pure @property @nogc @safe
    {
        return front() == TOK.endOfFile;
    }

    final TOK front() const pure @property @nogc @safe
    {
        return token.value;
    }

    final void popFront()
    {
        nextToken();
    }

    /// Returns: a newly allocated `Token`.
    Token* allocateToken() pure nothrow @safe
    {
        if (tokenFreelist)
        {
            Token* t = tokenFreelist;
            tokenFreelist = t.next;
            t.next = null;
            return t;
        }
        return new Token();
    }

    /// Frees the given token by returning it to the freelist.
    private void releaseToken(Token* token) pure nothrow @nogc @safe
    {
        if (mem.isGCEnabled)
            *token = Token.init;
        token.next = tokenFreelist;
        tokenFreelist = token;
    }

    final TOK nextToken()
    {
        prevloc = token.loc;
        if (token.next)
        {
            Token* t = token.next;
            memcpy(&token, t, Token.sizeof);
            releaseToken(t);
        }
        else
        {
            scan(&token);
        }
        //printf(token.toChars());
        return token.value;
    }

    /***********************
     * Look ahead at next token's value.
     */
    final TOK peekNext()
    {
        return peek(&token).value;
    }

    /***********************
     * Look 2 tokens ahead at value.
     */
    final TOK peekNext2()
    {
        Token* t = peek(&token);
        return peek(t).value;
    }

    /****************************
     * Turn next token in buffer into a token.
     * Params:
     *  t = the token to set the resulting Token to
     */
    final void scan(Token* t)
    {
        const lastLine = linnum;
        Loc startLoc;
        t.blockComment = null;
        t.lineComment = null;

        size_t universalCharacterName4, universalCharacterName8;

        while (1)
        {
            t.ptr = p;
            //printf("p = %p, *p = '%c'\n",p,*p);
            t.loc = loc();
            switch (*p)
            {
            case 0:
            case 0x1A:
                t.value = TOK.endOfFile; // end of file
                // Intentionally not advancing `p`, such that subsequent calls keep returning TOK.endOfFile.
                return;
            case ' ':
                // Skip 4 spaces at a time after aligning 'p' to a 4-byte boundary.
                while ((cast(size_t)p) % uint.sizeof)
                {
                    if (*p != ' ')
                        goto LendSkipFourSpaces;
                    p++;
                }
                while (*(cast(uint*)p) == 0x20202020) // ' ' == 0x20
                    p += 4;
                // Skip over any remaining space on the line.
                while (*p == ' ')
                    p++;
            LendSkipFourSpaces:
                version (DMDLIB)
                {
                    if (whitespaceToken)
                    {
                        t.value = TOK.whitespace;
                        return;
                    }
                }
                continue; // skip white space
            case '\t':
            case '\v':
            case '\f':
                p++;
                version (DMDLIB)
                {
                    if (whitespaceToken)
                    {
                        t.value = TOK.whitespace;
                        return;
                    }
                }
                continue; // skip white space
            case '\r':
                p++;
                if (*p != '\n') // if CR stands by itself
                {
                    endOfLine();
                    if (tokenizeNewlines)
                    {
                        t.value = TOK.endOfLine;
                        tokenizeNewlines = false;
                        return;
                    }
                }
                version (DMDLIB)
                {
                    if (whitespaceToken)
                    {
                        t.value = TOK.whitespace;
                        return;
                    }
                }
                continue; // skip white space
            case '\n':
                p++;
                endOfLine();
                if (tokenizeNewlines)
                {
                    t.value = TOK.endOfLine;
                    tokenizeNewlines = false;
                    return;
                }
                version (DMDLIB)
                {
                    if (whitespaceToken)
                    {
                        t.value = TOK.whitespace;
                        return;
                    }
                }
                continue; // skip white space

            case '\\':
                if (Ccompile)
                {
                    if (p[1] == '\r' || p[1] == '\n')
                    {
                        ++p; // ignore \ followed by new line, like VC does
                        continue;
                    }
                    else if (p[1] == 'u')
                    {
                        // Universal Character Name (C) 2 byte
                        // \uXXXX
                        // let the main case handling for identifiers process this

                        // case_indent will always increment, so subtract to prevent branching on the fast path
                        p--;

                        goto case_ident;
                    }
                    else if (p[1] == 'U')
                    {
                        // Universal Character Name (C) 4 byte
                        // \UXXXXXXXX
                        // let the main case handling for identifiers process this

                        // case_indent will always increment, so subtract to prevent branching on the fast path
                        p--;

                        goto case_ident;
                    }
                }
                goto default;

            case '0':
                if (!isZeroSecond(p[1]))        // if numeric literal does not continue
                {
                    ++p;
                    t.unsvalue = 0;
                    t.value = TOK.int32Literal;
                    return;
                }
                goto Lnumber;

            case '1': .. case '9':
                if (!isDigitSecond(p[1]))       // if numeric literal does not continue
                {
                    t.unsvalue = *p - '0';
                    ++p;
                    t.value = TOK.int32Literal;
                    return;
                }
            Lnumber:
                t.value = number(t);
                return;

            case '\'':
                if (issinglechar(p[1]) && p[2] == '\'')
                {
                    t.unsvalue = p[1];        // simple one character literal
                    t.value = TOK.charLiteral;
                    p += 3;
                }
                else if (Ccompile)
                {
                    clexerCharConstant(*t, 0);
                }
                else
                {
                    t.value = charConstant(t);
                }
                return;

            case 'u':
            case 'U':
            case 'L':
                if (!Ccompile)
                    goto case_ident;
                if (p[1] == '\'')       // C wide character constant
                {
                    char c = *p;
                    if (c == 'L')       // convert L to u or U
                        c = (wchar_tsize == 4) ? 'u' : 'U';
                    ++p;
                    clexerCharConstant(*t, c);
                    return;
                }
                if (p[1] == '\"')  // C wide string literal
                {
                    const c = *p;
                    ++p;
                    escapeStringConstant(t);
                    t.postfix = c == 'L' ? (wchar_tsize == 2 ? 'w' : 'd') :
                                c == 'u' ? 'w' :
                                'd';
                    return;
                }
                if (p[1] == '8' && p[2] == '\"') // C UTF-8 string literal
                {
                    p += 2;
                    escapeStringConstant(t);
                    return;
                }
                goto case_ident;

            case 'r':
                if (Ccompile || p[1] != '"')
                    goto case_ident;
                p++;
                goto case '`';
            case '`':
                if (Ccompile)
                    goto default;
                wysiwygStringConstant(t);
                return;
            case 'x':
                if (p[1] != '"')
                    goto case_ident;
                p++;
                t.value = hexStringConstant(t);
                return;
            case 'q':
                if (Ccompile)
                    goto case_ident;
                if (p[1] == '"')
                {
                    p++;
                    delimitedStringConstant(t);
                    return;
                }
                if (p[1] == '{')
                {
                    p++;
                    tokenStringConstant(t);
                    return;
                }
                goto case_ident;
            case 'i':
                if (Ccompile)
                    goto case_ident;
                if (p[1] == '"')
                {
                    p++; // skip the i
                    escapeStringConstant(t, true);
                    return;
                }
                if (p[1] == '`')
                {
                    p++; // skip the i
                    wysiwygStringConstant(t, true);
                    return;
                }
                if (p[1] == 'q' && p[2] == '{')
                {
                    p += 2; // skip the i and q
                    tokenStringConstant(t, true);
                    return;
                }
                goto case_ident;
            case '"':
                escapeStringConstant(t);
                return;
            case 'a':
            case 'b':
            case 'c':
            case 'd':
            case 'e':
            case 'f':
            case 'g':
            case 'h':
                /*case 'i':*/
            case 'j':
            case 'k':
            case 'l':
            case 'm':
            case 'n':
            case 'o':
            case 'p':
                /*case 'q': case 'r':*/
            case 's':
            case 't':
            //case 'u':
            case 'v':
            case 'w':
                /*case 'x':*/
            case 'y':
            case 'z':
            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'E':
            case 'F':
            case 'G':
            case 'H':
            case 'I':
            case 'J':
            case 'K':
            //case 'L':
            case 'M':
            case 'N':
            case 'O':
            case 'P':
            case 'Q':
            case 'R':
            case 'S':
            case 'T':
            //case 'U':
            case 'V':
            case 'W':
            case 'X':
            case 'Y':
            case 'Z':
            case '_':
            case_ident:
                {
        IdentLoop: while (1)
                    {
                        // If this is changed, change the decrement in C's universal character name code above
                        // For syntax \uXXXX and \UXXXXXXXX
                        const c = *++p;

                        // Is this the first character of the identifier
                        // For the universal character name this will line up,
                        //  for the main switch it won't since it wasn't the first,
                        //  for the default it won't either because a decode increments.
                        const isStartCharacter = t.ptr is p;

                        if (isidchar(c))
                            continue;
                        if (c & 0x80)
                        {
                            const s = p;
                            const u = decodeUTF();

                            if (isStartCharacter)
                            {
                                if (charLookup.isStart(u))
                                    continue;
                                error(t.loc, "character 0x%04x is not allowed as a start character in an identifier", u);
                            }
                            else
                            {
                                if (charLookup.isContinue(u))
                                    continue;
                                error(t.loc, "character 0x%04x is not allowed as a continue character in an identifier", u);
                            }

                            p = s;
                        }
                        else if (Ccompile && c == '\\')
                        {
                            uint times;
                            const s = p;
                            p++;

                            if (*p == 'u')
                            {
                                // Universal Character Name (C) 2 byte
                                // \uXXXX
                                p++;
                                times = 4;
                            }
                            else if (*p == 'U')
                            {
                                // Universal Character Name (C) 4 byte
                                // \UXXXXXXXX
                                p++;
                                times = 8;
                            }
                            else
                            {
                                error(t.loc, "char 0x%x is not allowed to follow '\\' expecting a C universal character name in format \\uXXXX or \\UXXXXXXXX with hex digits instead of X with invalid u/U", *p);
                                p = s;
                                break;
                            }

                            foreach(_; 0 .. times)
                            {
                                const hc = *p;
                                p++;

                                if ((hc >= '0' && hc <= '9') || (hc >= 'a' && hc <= 'f') || (hc >= 'A' && hc <= 'F'))
                                    continue;

                                error(t.loc, "char 0x%x is not allowed to follow '\\' expecting a C universal character name in format \\uXXXX or \\UXXXXXXXX with hex digits instead of X with invalid hex digit", hc);
                                p = s;
                                break IdentLoop;
                            }

                            continue;
                        }
                        break;
                    }

                    Identifier id;

                    if (universalCharacterName4 > 0 || universalCharacterName8 > 0)
                    {
                        auto priorValidation = t.ptr[0 .. p - t.ptr];
                        const(char)* priorVPtr = priorValidation.ptr;
                        const possibleLength = (
                            priorValidation.length - (
                                (universalCharacterName4 * 6) +
                                (universalCharacterName8 * 10)
                            )) + (
                                (universalCharacterName4 * 3) +
                                (universalCharacterName8 * 4)
                            );

                        char[64] buffer = void;
                        SmallBuffer!char sb = SmallBuffer!char(possibleLength, buffer[]);

                        char[] storage = sb.extent;
                        size_t offset;

                        while(priorVPtr < &priorValidation[$-1] + 1)
                        {
                            if (*priorVPtr == '\\')
                            {
                                dchar tempDchar = 0;
                                uint times;

                                // universal character name (C)
                                if (priorVPtr[1] == 'u')
                                    times = 4;
                                else if (priorVPtr[1] == 'U')
                                    times = 8;
                                else
                                    assert(0, "ICE: Universal character name is 2 or 4 bytes only");
                                priorVPtr += 2;

                                foreach(_; 0 .. times)
                                {
                                    char c = *++priorVPtr;
                                    if (c >= '0' && c <= '9')
                                        c -= '0';
                                    else if (c >= 'a' && c <= 'f')
                                        c -= 'a' - 10;
                                    else if (c >= 'A' && c <= 'F')
                                        c -= 'A' - 10;

                                    tempDchar <<= 4;
                                    tempDchar |= c;
                                }

                                utf_encodeChar(&storage[offset], tempDchar);
                                offset += utf_codeLengthChar(tempDchar);

                                // Could be an error instead of a warning,
                                //  but hey it was written specifically so why worry?
                                if (priorVPtr is priorValidation.ptr)
                                {
                                    if (!charLookup.isStart(tempDchar))
                                        warning(t.loc, "char 0x%x is not allowed start character for an identifier", tempDchar);
                                }
                                else
                                {
                                    if (!charLookup.isContinue(tempDchar))
                                        warning(t.loc, "char 0x%x is not allowed continue character for an identifier", tempDchar);
                                }
                            }
                            else
                                storage[offset++] = *++priorVPtr;
                        }

                        id = Identifier.idPool(storage[0 .. offset], false);
                    }
                    else
                        id = Identifier.idPool((cast(char*)t.ptr)[0 .. p - t.ptr], false);

                    t.ident = id;
                    t.value = cast(TOK)id.getValue();

                    anyToken = 1;

                    /* Different keywords for C and D
                     */
                    if (Ccompile)
                    {
                        if (t.value != TOK.identifier)
                        {
                            t.value = Ckeywords[t.value];  // filter out D keywords
                        }
                    }
                    else if (t.value >= FirstCKeyword)
                        t.value = TOK.identifier;       // filter out C keywords

                    else if (*t.ptr == '_') // if special identifier token
                    {
                        void toToken(const(char)[] s)
                        {
                            t.value = TOK.string_;
                            t.ustring = s.ptr;
                            t.len = cast(uint)s.length;
                            t.postfix = 0;
                        }

                        if (id == Id.DATE)
                            toToken(compileEnv.date);
                        else if (id == Id.TIME)
                            toToken(compileEnv.time);
                        else if (id == Id.VENDOR)
                            toToken(compileEnv.vendor);
                        else if (id == Id.TIMESTAMP)
                            toToken(compileEnv.timestamp);
                        else if (id == Id.VERSIONX)
                        {
                            t.value = TOK.int64Literal;
                            t.unsvalue = compileEnv.versionNumber;
                        }
                        else if (id == Id.EOFX)
                        {
                            t.value = TOK.endOfFile;
                            // Advance scanner to end of file
                            while (!(*p == 0 || *p == 0x1A))
                                p++;
                        }
                    }
                    //printf("t.value = %d\n",t.value);
                    return;
                }
            case '/':
                p++;
                switch (*p)
                {
                case '=':
                    p++;
                    t.value = TOK.divAssign;
                    return;
                case '*':
                    p++;
                    startLoc = loc();
                    while (1)
                    {
                        while (1)
                        {
                            const c = *p;
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
                                error(t.loc, "unterminated /* */ comment");
                                //p = end;
                                t.loc = loc();
                                t.value = TOK.endOfFile;
                                return;
                            default:
                                if (c & 0x80)
                                {
                                    const u = decodeUTF();
                                    if (u == PS || u == LS)
                                        endOfLine();
                                }
                                p++;
                                continue;
                            }
                            break;
                        }
                        p++;
                        if (p[-2] == '*' && p - 3 != t.ptr)
                            break;
                    }
                    if (commentToken)
                    {
                        t.loc = startLoc;
                        t.value = TOK.comment;
                        return;
                    }
                    if (doDocComment && t.ptr[2] == '*' && p - 4 != t.ptr)
                    {
                        // if /** but not /**/
                        getDocComment(t, lastLine == startLoc.linnum, startLoc.linnum - lastDocLine > 1);
                        lastDocLine = linnum;
                    }
                    continue;
                case '/': // do // style comments
                    startLoc = loc();
                    while (1)
                    {
                        const c = *++p;
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
                                t.loc = startLoc;
                                t.value = TOK.comment;
                                return;
                            }
                            if (doDocComment && t.ptr[2] == '/')
                            {
                                getDocComment(t, lastLine == startLoc.linnum, startLoc.linnum - lastDocLine > 1);
                                lastDocLine = linnum;
                            }
                            //p = end;
                            t.loc = loc();
                            t.value = TOK.endOfFile;
                            return;
                        default:
                            if (c & 0x80)
                            {
                                const u = decodeUTF();
                                if (u == PS || u == LS)
                                    break;
                            }
                            continue;
                        }
                        break;
                    }
                    if (commentToken)
                    {
                        version (DMDLIB) {}
                        else
                        {
                            p++;
                            endOfLine();
                        }
                        t.loc = startLoc;
                        t.value = TOK.comment;
                        return;
                    }
                    if (doDocComment && t.ptr[2] == '/')
                    {
                        getDocComment(t, lastLine == startLoc.linnum, startLoc.linnum - lastDocLine > 1);
                        lastDocLine = linnum;
                    }
                    p++;
                    endOfLine();
                    continue;
                case '+':
                    if (!Ccompile)
                    {
                        int nest;
                        startLoc = loc();
                        p++;
                        nest = 1;
                        while (1)
                        {
                            char c = *p;
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
                                error(t.loc, "unterminated /+ +/ comment");
                                //p = end;
                                t.loc = loc();
                                t.value = TOK.endOfFile;
                                return;
                            default:
                                if (c & 0x80)
                                {
                                    uint u = decodeUTF();
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
                            t.loc = startLoc;
                            t.value = TOK.comment;
                            return;
                        }
                        if (doDocComment && t.ptr[2] == '+' && p - 4 != t.ptr)
                        {
                            // if /++ but not /++/
                            getDocComment(t, lastLine == startLoc.linnum, startLoc.linnum - lastDocLine > 1);
                            lastDocLine = linnum;
                        }
                        continue;
                    }
                    break;
                default:
                    break;
                }
                t.value = TOK.div;
                return;
            case '.':
                p++;
                if (isdigit(*p))
                {
                    /* Note that we don't allow ._1 and ._ as being
                     * valid floating point numbers.
                     */
                    p--;
                    t.value = inreal(t);
                }
                else if (p[0] == '.')
                {
                    if (p[1] == '.')
                    {
                        p += 2;
                        t.value = TOK.dotDotDot;
                    }
                    else
                    {
                        p++;
                        t.value = TOK.slice;
                    }
                }
                else
                    t.value = TOK.dot;
                return;
            case '&':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.andAssign;
                }
                else if (*p == '&')
                {
                    p++;
                    t.value = TOK.andAnd;
                }
                else
                    t.value = TOK.and;
                return;
            case '|':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.orAssign;
                }
                else if (*p == '|')
                {
                    p++;
                    t.value = TOK.orOr;
                }
                else
                    t.value = TOK.or;
                return;
            case '-':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.minAssign;
                }
                else if (*p == '-')
                {
                    p++;
                    t.value = TOK.minusMinus;
                }
                else if (*p == '>')
                {
                    ++p;
                    t.value = TOK.arrow;
                }
                else
                    t.value = TOK.min;
                return;
            case '+':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.addAssign;
                }
                else if (*p == '+')
                {
                    p++;
                    t.value = TOK.plusPlus;
                }
                else
                    t.value = TOK.add;
                return;
            case '<':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.lessOrEqual; // <=
                }
                else if (*p == '<')
                {
                    p++;
                    if (*p == '=')
                    {
                        p++;
                        t.value = TOK.leftShiftAssign; // <<=
                    }
                    else
                        t.value = TOK.leftShift; // <<
                }
                else if (*p == ':' && Ccompile)
                {
                    ++p;
                    t.value = TOK.leftBracket;  // <:
                }
                else if (*p == '%' && Ccompile)
                {
                    ++p;
                    t.value = TOK.leftCurly;    // <%
                }
                else
                    t.value = TOK.lessThan; // <
                return;
            case '>':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.greaterOrEqual; // >=
                }
                else if (*p == '>')
                {
                    p++;
                    if (*p == '=')
                    {
                        p++;
                        t.value = TOK.rightShiftAssign; // >>=
                    }
                    else if (*p == '>')
                    {
                        p++;
                        if (*p == '=')
                        {
                            p++;
                            t.value = TOK.unsignedRightShiftAssign; // >>>=
                        }
                        else
                            t.value = TOK.unsignedRightShift; // >>>
                    }
                    else
                        t.value = TOK.rightShift; // >>
                }
                else
                    t.value = TOK.greaterThan; // >
                return;
            case '!':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.notEqual; // !=
                }
                else
                    t.value = TOK.not; // !
                return;
            case '=':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.equal; // ==
                }
                else if (*p == '>')
                {
                    p++;
                    t.value = TOK.goesTo; // =>
                }
                else
                    t.value = TOK.assign; // =
                return;
            case '~':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.concatenateAssign; // ~=
                }
                else
                    t.value = TOK.tilde; // ~
                return;
            case '^':
                p++;
                if (*p == '^')
                {
                    p++;
                    if (*p == '=')
                    {
                        p++;
                        t.value = TOK.powAssign; // ^^=
                    }
                    else
                        t.value = TOK.pow; // ^^
                }
                else if (*p == '=')
                {
                    p++;
                    t.value = TOK.xorAssign; // ^=
                }
                else
                    t.value = TOK.xor; // ^
                return;
            case '(':
                p++;
                t.value = TOK.leftParenthesis;
                return;
            case ')':
                p++;
                t.value = TOK.rightParenthesis;
                return;
            case '[':
                p++;
                t.value = TOK.leftBracket;
                return;
            case ']':
                p++;
                t.value = TOK.rightBracket;
                return;
            case '{':
                p++;
                t.value = TOK.leftCurly;
                return;
            case '}':
                p++;
                t.value = TOK.rightCurly;
                return;
            case '?':
                p++;
                t.value = TOK.question;
                return;
            case ',':
                p++;
                t.value = TOK.comma;
                return;
            case ';':
                p++;
                t.value = TOK.semicolon;
                return;
            case ':':
                p++;
                if (*p == ':')
                {
                    ++p;
                    t.value = TOK.colonColon;
                }
                else if (*p == '>' && Ccompile)
                {
                    ++p;
                    t.value = TOK.rightBracket;
                }
                else
                    t.value = TOK.colon;
                return;
            case '$':
                p++;
                t.value = TOK.dollar;
                return;
            case '@':
                p++;
                t.value = TOK.at;
                return;
            case '*':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.mulAssign;
                }
                else
                    t.value = TOK.mul;
                return;
            case '%':
                p++;
                if (*p == '=')
                {
                    p++;
                    t.value = TOK.modAssign;
                }
                else if (*p == '>' && Ccompile)
                {
                    ++p;
                    t.value = TOK.rightCurly;
                }
                else if (*p == ':' && Ccompile)
                {
                    goto case '#';      // %: means #
                }
                else
                    t.value = TOK.mod;
                return;
            case '#':
                {
                    // https://issues.dlang.org/show_bug.cgi?id=22825
                    // Special token sequences are terminated by newlines,
                    // and should not be skipped over.
                    this.tokenizeNewlines = true;
                    p++;
                    if (parseSpecialTokenSequence())
                        continue;
                    t.value = TOK.pound;
                    return;
                }
            default:
                {
                    dchar c = *p;
                    if (c & 0x80)
                    {
                        c = decodeUTF();

                        // Check for start of an identifier
                        if (charLookup.isStart(c))
                            goto case_ident;

                        if (c == PS || c == LS)
                        {
                            endOfLine();
                            p++;
                            if (tokenizeNewlines)
                            {
                                t.value = TOK.endOfLine;
                                tokenizeNewlines = false;
                                return;
                            }
                            continue;
                        }
                    }
                    if (c < 0x80 && isprint(c))
                        error(t.loc, "character '%c' is not a valid token", c);
                    else
                        error(t.loc, "character 0x%02x is not a valid token", c);
                    p++;
                    continue;
                    // assert(0);
                }
            }
        }
    }

    final Token* peek(Token* ct)
    {
        Token* t;
        if (ct.next)
            t = ct.next;
        else
        {
            t = allocateToken();
            scan(t);
            ct.next = t;
        }
        return t;
    }

    /*********************************
     * tk is on the opening (.
     * Look ahead and return token that is past the closing ).
     */
    final Token* peekPastParen(Token* tk)
    {
        //printf("peekPastParen()\n");
        int parens = 1;
        int curlynest = 0;
        while (1)
        {
            tk = peek(tk);
            //tk.print();
            switch (tk.value)
            {
            case TOK.leftParenthesis:
                parens++;
                continue;
            case TOK.rightParenthesis:
                --parens;
                if (parens)
                    continue;
                tk = peek(tk);
                break;
            case TOK.leftCurly:
                curlynest++;
                continue;
            case TOK.rightCurly:
                if (--curlynest >= 0)
                    continue;
                break;
            case TOK.semicolon:
                if (curlynest)
                    continue;
                break;
            case TOK.endOfFile:
                break;
            default:
                continue;
            }
            return tk;
        }
    }

    /*******************************************
     * Parse escape sequence.
     */
    private uint escapeSequence(out dchar c2)
    {
        return Lexer.escapeSequence(token.loc, p, Ccompile, c2);
    }

    /********
     * Parse the given string literal escape sequence into a single character.
     * D https://dlang.org/spec/lex.html#escape_sequences
     * C11 6.4.4.4
     * Params:
     *  loc = location to use for error messages
     *  sequence = pointer to string with escape sequence to parse. Updated to
     *             point past the end of the escape sequence
     *  Ccompile = true for compile C11 escape sequences
     *  c2 = returns second `dchar` of html entity with 2 code units, otherwise stays `dchar.init`
     * Returns:
     *  the escape sequence as a single character
     */
    private dchar escapeSequence(Loc loc, ref const(char)* sequence, bool Ccompile, out dchar c2)
    {
        const(char)* p = sequence; // cache sequence reference on stack
        scope(exit) sequence = p;

        uint c = *p;
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
        case 'a':
            c = 7;
            goto Lconsume;
        case 'b':
            c = 8;
            goto Lconsume;
        case 'f':
            c = 12;
            goto Lconsume;
        case 'n':
            c = 10;
            goto Lconsume;
        case 'r':
            c = 13;
            goto Lconsume;
        case 't':
            c = 9;
            goto Lconsume;
        case 'v':
            c = 11;
            goto Lconsume;
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
            if (ishex(cast(char)c))
            {
                uint v = 0;
                int n = 0;
                if (Ccompile && ndigits == 2)
                {
                    /* C11 6.4.4.4-7 one to infinity hex digits
                     */
                    do
                    {
                        if (isdigit(cast(char)c))
                            c -= '0';
                        else if (islower(c))
                            c -= 'a' - 10;
                        else
                            c -= 'A' - 10;
                        v = v * 16 + c;
                        c = *++p;
                    } while (ishex(cast(char)c));
                }
                else
                {
                    while (1)
                    {
                        if (isdigit(cast(char)c))
                            c -= '0';
                        else if (islower(c))
                            c -= 'a' - 10;
                        else
                            c -= 'A' - 10;
                        v = v * 16 + c;
                        c = *++p;
                        if (++n == ndigits)
                            break;
                        if (!ishex(cast(char)c))
                        {
                            error(loc, "escape hex sequence has %d hex digits instead of %d", n, ndigits);
                            break;
                        }
                    }
                    if (ndigits != 2 && !utf_isValidDchar(v))
                    {
                        error(loc, "invalid UTF character \\U%08x", v);
                        if (v >= 0xD800 && v <= 0xDFFF)
                            errorSupplemental("The code unit is a UTF-16 surrogate, is the escape UTF-16 not a Unicode code point?");
                        v = '?'; // recover with valid UTF character
                    }
                }
                c = v;
            }
            else
            {
                error(loc, "undefined escape hex sequence \\%c%c", sequence[0], c);
                p++;
            }
            break;
        case '&':
            if (Ccompile)
                goto default;

            // named character entity
            for (const idstart = ++p; 1; p++)
            {
                switch (*p)
                {
                case ';':
                    auto entity = HtmlNamedEntity(idstart[0 .. p - idstart]);
                    c = entity[0];
                    if (entity == entity.init)
                    {
                        error(loc, "unnamed character entity &%.*s;", cast(int)(p - idstart), idstart);
                        c = '?';
                    }
                    if (entity[1] != entity.init[1])
                        c2 = entity[1];

                    p++;
                    break;
                default:
                    if (isalpha(*p) || (p != idstart && isdigit(*p)))
                        continue;
                    error(loc, "unterminated named entity &%.*s;", cast(int)(p - idstart + 1), idstart);
                    c = '?';
                    break;
                }
                break;
            }
            break;
        case 0:
        case 0x1A:
            // end of file
            c = '\\';
            break;
        default:
            if (isoctal(cast(char)c))
            {
                uint v = 0;
                int n = 0;
                do
                {
                    v = v * 8 + (c - '0');
                    c = *++p;
                }
                while (++n < 3 && isoctal(cast(char)c));
                c = v;
                if (c > 0xFF)
                    error(loc, "escape octal sequence \\%03o is larger than \\377", c);
            }
            else
            {
                error(loc, "undefined escape sequence \\%c", c);
                p++;
            }
            break;
        }
        return c;
    }

    /**
    Lex a wysiwyg string. `p` must be pointing to the first character before the
    contents of the string literal. The character pointed to by `p` will be used as
    the terminating character (i.e. backtick or double-quote).
    Params:
        result = pointer to the token that accepts the result
    */
    private void wysiwygStringConstant(Token* result, bool supportInterpolation = false)
    {
        if (supportInterpolation)
        {
            result.value = TOK.interpolated;
            result.interpolatedSet = null;
        }
        else
        {
            result.value = TOK.string_;
        }

        Loc start = loc();
        auto terminator = p[0];
        p++;
        stringbuffer.setsize(0);
        while (1)
        {
            dchar c = p[0];
            p++;
            switch (c)
            {
            case '\n':
                endOfLine();
                break;
            case '\r':
                if (p[0] == '\n')
                    continue; // ignore
                c = '\n'; // treat EndOfLine as \n character
                endOfLine();
                break;
            case '$':
                if (!supportInterpolation)
                    goto default;

                if (!handleInterpolatedSegment(result, start))
                    goto default;

                continue;
            case 0:
            case 0x1A:
                error("unterminated string constant starting at %s", start.toChars());
                result.setString();
                // rewind `p` so it points to the EOF character
                p--;
                return;
            default:
                if (c == terminator)
                {
                    if (supportInterpolation)
                        result.appendInterpolatedPart(stringbuffer);
                    else
                        result.setString(stringbuffer);

                    stringPostfix(result);
                    return;
                }
                else if (c & 0x80)
                {
                    p--;
                    const u = decodeUTF();
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
    final TOK hexStringConstant(Token* t)
    {
        Loc start = loc();
        uint n = 0;
        uint v = ~0; // dead assignment, needed to suppress warning
        p++;
        stringbuffer.setsize(0);
        while (1)
        {
            dchar c = *p++;
            switch (c)
            {
            case ' ':
            case '\t':
            case '\v':
            case '\f':
                continue; // skip white space
            case '\r':
                if (*p == '\n')
                    continue; // ignore '\r' if followed by '\n'
                // Treat isolated '\r' as if it were a '\n'
                goto case '\n';
            case '\n':
                endOfLine();
                continue;
            case 0:
            case 0x1A:
                error("unterminated string constant starting at %s", start.toChars());
                t.setString();
                // decrement `p`, because it needs to point to the next token (the 0 or 0x1A character is the TOK.endOfFile token).
                p--;
                return TOK.hexadecimalString;
            case '"':
                if (n & 1)
                {
                    error("odd number (%d) of hex characters in hex string", n);
                    stringbuffer.writeByte(v);
                }
                t.setString(stringbuffer);
                stringPostfix(t);
                return TOK.hexadecimalString;
            default:
                if (c >= '0' && c <= '9')
                    c -= '0';
                else if (c >= 'a' && c <= 'f')
                    c -= 'a' - 10;
                else if (c >= 'A' && c <= 'F')
                    c -= 'A' - 10;
                else if (c & 0x80)
                {
                    p--;
                    const u = decodeUTF();
                    p++;
                    if (u == PS || u == LS)
                        endOfLine();
                    else
                        error("non-hex character \\u%04x in hex string", u);
                }
                else
                    error("non-hex character '%c' in hex string", c);
                if (n & 1)
                {
                    v = (v << 4) | c;
                    stringbuffer.writeByte(v);
                }
                else
                    v = c;
                n++;
                break;
            }
        }
        assert(0); // see bug 15731
    }

    /**
    Lex a delimited string. Some examples of delimited strings are:
    ---
    q"(foo(xxx))"      // "foo(xxx)"
    q"[foo$(LPAREN)]"  // "foo$(LPAREN)"
    q"/foo]/"          // "foo]"
    q"HERE
    foo
    HERE"              // "foo\n"
    ---
    It is assumed that `p` points to the opening double-quote '"'.
    Params:
        result = pointer to the token that accepts the result
    */
    private void delimitedStringConstant(Token* result)
    {
        result.value = TOK.string_;
        Loc start = loc();
        dchar delimleft = 0;
        dchar delimright = 0;
        uint nest = 1;
        uint nestcount = ~0; // dead assignment, needed to suppress warning
        Identifier hereid = null;
        uint blankrol = 0;
        uint startline = 0;
        p++;
        stringbuffer.setsize(0);
        while (1)
        {
            const s = p;
            dchar c = *p++;
            //printf("c = '%c'\n", c);
            switch (c)
            {
            case '\n':
            Lnextline:
                endOfLine();
                startline = 1;
                if (blankrol)
                {
                    blankrol = 0;
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
                    continue; // ignore
                c = '\n'; // treat EndOfLine as \n character
                goto Lnextline;
            case 0:
            case 0x1A:
                error("unterminated delimited string constant starting at %s", start.toChars());
                result.setString();
                // decrement `p`, because it needs to point to the next token (the 0 or 0x1A character is the TOK.endOfFile token).
                p--;
                return;
            default:
                if (c & 0x80)
                {
                    p--;
                    c = decodeUTF();
                    p++;
                    if (c == PS || c == LS)
                        goto Lnextline;
                }
                break;
            }
            if (delimleft == 0)
            {
                delimleft = c;
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
                else if (isalpha(c) || c == '_' || (c >= 0x80 && charLookup.isStart(c)))
                {
                    // Start of identifier; must be a heredoc
                    Token tok;
                    p = s;
                    scan(&tok); // read in heredoc identifier
                    if (tok.value != TOK.identifier)
                    {
                        error("identifier expected for heredoc, not %s", tok.toChars());
                        delimright = c;
                    }
                    else
                    {
                        hereid = tok.ident;
                        //printf("hereid = '%s'\n", hereid.toChars());
                        blankrol = 1;
                    }
                    nest = 0;
                }
                else
                {
                    delimright = c;
                    nest = 0;
                    if (isspace(c))
                        error("delimiter cannot be whitespace");
                }
            }
            else
            {
                if (blankrol)
                {
                    error("heredoc rest of line should be blank");
                    blankrol = 0;
                    continue;
                }
                if (nest == 1)
                {
                    if (c == delimleft)
                        nestcount++;
                    else if (c == delimright)
                    {
                        nestcount--;
                        if (nestcount == 0)
                            goto Ldone;
                    }
                }
                else if (c == delimright)
                    goto Ldone;

                // we're looking for a new identifier token
                if (startline && (isalpha(c) || c == '_' || (c >= 0x80 && charLookup.isStart(c))) && hereid)
                {
                    Token tok;
                    auto psave = p;
                    p = s;
                    scan(&tok); // read in possible heredoc identifier
                    //printf("endid = '%s'\n", tok.ident.toChars());
                    if (tok.value == TOK.identifier && tok.ident is hereid)
                    {
                        /* should check that rest of line is blank
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
            error("delimited string must end in `%s\"`", hereid.toChars());
        else if (isspace(delimright))
            error("delimited string must end in `\"`");
        else
            error(token.loc, "delimited string must end in `%c\"`", delimright);
        result.setString(stringbuffer);
        stringPostfix(result);
    }

    /**
    Lex a token string. Some examples of token strings are:
    ---
    q{ foo(xxx) }    // " foo(xxx) "
    q{foo$(LPAREN)}  // "foo$(LPAREN)"
    q{{foo}"}"}      // "{foo}"}""
    ---
    It is assumed that `p` points to the opening curly-brace.
    Params:
        result = pointer to the token that accepts the result
    */
    private void tokenStringConstant(Token* result, bool supportInterpolation = false)
    {
        if (supportInterpolation)
        {
            result.value = TOK.interpolated;
            result.interpolatedSet = null;
        }
        else
        {
            result.value = TOK.string_;
        }

        uint nest = 1;
        const start = loc();
        auto pstart = ++p;
        inTokenStringConstant++;
        scope(exit) inTokenStringConstant--;
        while (1)
        {
            Token tok;
            scan(&tok);
            switch (tok.value)
            {
            case TOK.leftCurly:
                nest++;
                continue;
            case TOK.rightCurly:
                if (--nest == 0)
                {
                    if (supportInterpolation)
                        result.appendInterpolatedPart(pstart, p - 1 - pstart);
                    else
                        result.setString(pstart, p - 1 - pstart);

                    stringPostfix(result);
                    return;
                }
                continue;
            case TOK.dollar:
                if (!supportInterpolation)
                    goto default;

                stringbuffer.setsize(0);
                stringbuffer.write(pstart, p - 1 - pstart);
                if (!handleInterpolatedSegment(result, start))
                    goto default;

                stringbuffer.setsize(0);

                pstart = p;

                continue;
            case TOK.endOfFile:
                error("unterminated token string constant starting at %s", start.toChars());
                result.setString();
                return;
            default:
                continue;
            }
        }
    }

    // returns true if it got special treatment as an interpolated segment
    // otherwise returns false, indicating to treat it as just part of a normal string
    private bool handleInterpolatedSegment(Token* token, Loc start)
    {
        switch(*p)
        {
        case '(':
            // expression, at this level we need to scan until the closing ')'

            // always put the string part in first
            token.appendInterpolatedPart(stringbuffer);
            stringbuffer.setsize(0);

            int openParenCount = 1;
            p++; // skip the first open paren
            auto pstart = p;
            while (openParenCount > 0)
            {
                // need to scan with the lexer to support embedded strings and other complex cases
                Token tok;
                scan(&tok);
                if (tok.value == TOK.leftParenthesis)
                    openParenCount++;
                if (tok.value == TOK.rightParenthesis)
                    openParenCount--;
                if (tok.value == TOK.endOfFile)
                {
                    // FIXME: make this error better, it spams a lot
                    error("unterminated interpolated string constant starting at %s", start.toChars());
                    return false;
                }
            }

            // then put the interpolated string segment
            token.appendInterpolatedPart(pstart[0 .. p - 1 - pstart]);

            stringbuffer.setsize(0); // make sure this is reset from the last token scan
            // otherwise something like i"$(func("thing")) stuff" can still include it

            return true;
        default:
            // nothing special
            return false;
        }
    }

    /**
    Scan a quoted string while building the processed string value by
    handling escape sequences. The result is returned in the given `t` token.
    This function assumes that `p` currently points to the opening quote
    of the string.
    Params:
        t = the token to set the resulting string to
    * References:
    *   D https://dlang.org/spec/lex.html#double_quoted_strings
    *   ImportC C11 6.4.5
    */
    private void escapeStringConstant(Token* t, bool supportInterpolation = false)
    {
        if (supportInterpolation)
        {
            t.value = TOK.interpolated;
            t.interpolatedSet = null;
        }
        else
        {
            t.value = TOK.string_;
        }

        const start = loc();
        const tc = *p++;        // opening quote
        stringbuffer.setsize(0);
        while (1)
        {
            dchar c = *p++;
            dchar c2;
            switch (c)
            {
            case '\\':
                switch (*p)
                {
                case '&':
                    if (Ccompile)
                        goto default;

                    c = escapeSequence(c2);
                    stringbuffer.writeUTF8(c);
                    if (c2 != dchar.init)
                        stringbuffer.writeUTF8(c2);
                    continue;
                case 'u':
                case 'U':
                    c = escapeSequence(c2);
                    stringbuffer.writeUTF8(c);
                    continue;
                case '$':
                    if (supportInterpolation)
                    {
                        p++; // skip escaped $
                        stringbuffer.writeByte('$');
                        continue;
                    }
                    else
                        goto default;
                default:
                    c = escapeSequence(c2);
                    break;
                }
                break;
            case '$':
                if (!supportInterpolation)
                    goto default;

                if (!handleInterpolatedSegment(t, start))
                    goto default;

                continue;
            case '\n':
                endOfLine();
                if (Ccompile)
                    goto Lunterminated;
                break;
            case '\r':
                if (*p == '\n')
                    continue; // ignore
                c = '\n'; // treat EndOfLine as \n character
                endOfLine();
                if (Ccompile)
                    goto Lunterminated;
                break;
            case '\'':
            case '"':
                if (c != tc)
                    goto default;
                if (supportInterpolation)
                    t.appendInterpolatedPart(stringbuffer);
                else
                    t.setString(stringbuffer);
                if (!Ccompile)
                    stringPostfix(t);
                return;
            case 0:
            case 0x1A:
                // decrement `p`, because it needs to point to the next token (the 0 or 0x1A character is the TOK.endOfFile token).
                p--;
            Lunterminated:
                error("unterminated string constant starting at %s", start.toChars());
                t.setString();
                return;
            default:
                if (c & 0x80)
                {
                    p--;
                    c = decodeUTF();
                    if (c == LS || c == PS)
                    {
                        c = '\n';
                        endOfLine();
                        if (Ccompile)
                            goto Lunterminated;
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
     * Reference:
     *    https://dlang.org/spec/lex.html#characterliteral
     */
    private TOK charConstant(Token* t)
    {
        TOK tk = TOK.charLiteral;
        //printf("Lexer::charConstant\n");
        p++;
        dchar c = *p++;
        dchar c2;
        switch (c)
        {
        case '\\':
            switch (*p)
            {
            case 'u':
                tk = TOK.wcharLiteral;
                goto default;
            case 'U':
            case '&':
                tk = TOK.dcharLiteral;
                goto default;
            default:
                t.unsvalue = escapeSequence(c2);
                if (c2 != c2.init)
                {
                    error("html entity requires 2 code units, use a string instead of a character");
                    t.unsvalue = '?';
                }
                break;
            }
            break;
        case '\n':
        L1:
            endOfLine();
            goto case;
        case '\r':
            goto case '\'';
        case 0:
        case 0x1A:
            // decrement `p`, because it needs to point to the next token (the 0 or 0x1A character is the TOK.endOfFile token).
            p--;
            goto case;
        case '\'':
            error("unterminated character constant");
            t.unsvalue = '?';
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
                    tk = TOK.wcharLiteral;
                else
                    tk = TOK.dcharLiteral;
            }
            t.unsvalue = c;
            break;
        }
        if (*p != '\'')
        {
            while (*p != '\'' && *p != 0x1A && *p != 0 && *p != '\n' &&
                    *p != '\r' && *p != ';' && *p != ')' && *p != ']' && *p != '}')
            {
                if (*p & 0x80)
                {
                    const s = p;
                    c = decodeUTF();
                    if (c == LS || c == PS)
                    {
                        p = s;
                        break;
                    }
                }
                p++;
            }

            if (*p == '\'')
            {
                error("character constant has multiple characters");
                p++;
            }
            else
                error("unterminated character constant");
            t.unsvalue = '?';
            return tk;
        }
        p++;
        return tk;
    }

    /***************************************
     * Lex C character constant.
     * Parser is on the opening quote.
     * Params:
     *  t = token to fill in
     *  prefix = one of `u`, `U` or 0.
     * Reference:
     *  C11 6.4.4.4
     */
    private void clexerCharConstant(ref Token t, char prefix)
    {
        escapeStringConstant(&t);
        const(char)[] str = t.ustring[0 .. t.len];
        const n = str.length;
        const loc = t.loc;
        if (n == 0)
        {
            error(loc, "empty character constant");
            t.value = TOK.semicolon;
            return;
        }

        uint u;
        switch (prefix)
        {
            case 0:
                if (n == 1) // fast case
                {
                    u = str[0];
                }
                else if (n > 4)
                    error(loc, "max number of chars in character literal is 4, had %d",
                        cast(int)n);
                else
                {
                    foreach (i, c; str)
                        (cast(char*)&u)[n - 1 - i] = c;
                }
                break;

            case 'u':
                dchar d1;
                size_t idx;
                while (idx < n)
                {
                    string msg = utf_decodeChar(str, idx, d1);
                    if (msg)
                        error(loc, "%.*s", cast(int)msg.length, msg.ptr);
                }
                if (d1 >= 0x1_0000)
                    error(loc, "x%x does not fit in 16 bits", d1);
                t.unsvalue = d1;
                t.value = TOK.wcharLiteral; // C11 6.4.4.4-9
                return;

            case 'U':
                dchar d;
                size_t idx;
                auto msg = utf_decodeChar(str, idx, d);
                if (msg)
                    error(loc, "%.*s", cast(int)msg.length, msg.ptr);
                else if (idx < n)
                    error(loc, "max number of chars in 32 bit character literal is 1, had %d",
                        cast(int)((n + 3) >> 2));
                t.unsvalue = d;
                t.value = TOK.dcharLiteral; // C11 6.4.4.4-9
                return;

            default:
                assert(0);
        }
        t.value = n == 1 ? TOK.charLiteral : TOK.int32Literal;
        t.unsvalue = u;
    }

    /***************************************
     * Get postfix of string literal.
     */
    private void stringPostfix(Token* t) pure @nogc
    {
        switch (*p)
        {
        case 'c':
        case 'w':
        case 'd':
            t.postfix = *p;
            p++;
            break;
        default:
            t.postfix = 0;
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
    private TOK number(Token* t)
    {
        int base = 10;
        const start = p;
        ulong n = 0; // unsigned >=64 bit integer type
        int d;
        bool err = false;
        bool overflow = false;
        bool anyBinaryDigitsNoSingleUS = false;
        bool anyHexDigitsNoSingleUS = false;
        char errorDigit = 0;
        dchar c = *p;
        if (c == '0')
        {
            ++p;
            c = *p;
            switch (c)
            {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
                base = 8;
                break;

            case '8':
            case '9':
                errorDigit = cast(char) c;
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
                {
                    if (Ccompile && (p[1] == 'f' || p[1] == 'F' || p[1] == 'l' || p[1] == 'L'))
                        goto Lreal;  // if `0.f` or `0.L`
                    goto Ldone; // if ".identifier" or ".unicode"
                }
                goto Lreal; // '.' is part of current token
            case 'i':
            case 'f':
            case 'F':
                goto Lreal;
            case '_':
                if (Ccompile)
                    error("embedded `_` not allowed");
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
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                ++p;
                d = c - '0';
                break;
            case 'a':
            case 'b':
            case 'c':
            case 'd':
            case 'e':
            case 'f':
            case 'A':
            case 'B':
            case 'C':
            case 'D':
            case 'E':
            case 'F':
                ++p;
                if (base != 16)
                {
                    if (c == 'e' || c == 'E' || c == 'f' || c == 'F')
                        goto Lreal;
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
                if (base <= 10 && n > 0 && (isalpha(p[1]) || p[1] == '_' || p[1] & 0x80))
                {
                    if (Ccompile && base == 10 &&
                        (p[1] == 'e' || p[1] == 'E' || p[1] == 'f' || p[1] == 'F' || p[1] == 'l' || p[1] == 'L'))
                        goto Lreal;  // if `1.e6` or `1.f` or `1.L`
                    goto Ldone; // if ".identifier" or ".unicode"
                }
                if (base == 16 && (!ishex(p[1]) || p[1] == '_' || p[1] & 0x80))
                    goto Ldone; // if ".identifier" or ".unicode"
                if (base == 2)
                    goto Ldone; // if ".identifier" or ".unicode"
                goto Lreal; // otherwise as part of a floating point literal

            case 'i':
                if (Ccompile)
                    goto Ldone;
                goto Lreal;

            case 'p':
            case 'P':
            Lreal:
                p = start;
                return inreal(t);
            case '_':
                if (Ccompile)
                    goto default;
                ++p;
                continue;
            default:
                goto Ldone;
            }
            // got a digit here, set any necessary flags, check for errors
            anyHexDigitsNoSingleUS = true;
            anyBinaryDigitsNoSingleUS = true;
            if (!errorDigit && d >= base)
            {
                errorDigit = cast(char) c;
            }
            // Avoid expensive overflow check if we aren't at risk of overflow
            if (n <= 0x0FFF_FFFF_FFFF_FFFFUL)
                n = n * base + d;
            else
            {
                import core.checkedint : mulu, addu;

                n = mulu(n, base, overflow);
                n = addu(n, d, overflow);
            }
        }
    Ldone:
        if (errorDigit)
        {
            error(scanloc, "%s digit expected, not `%c`", base == 2 ? "binary".ptr :
                                                 base == 8 ? "octal".ptr :
                                                 "decimal".ptr, errorDigit);
            err = true;
        }
        if (overflow && !err)
        {
            error(scanloc, "integer overflow");
            err = true;
        }
        if ((base == 2 && !anyBinaryDigitsNoSingleUS) ||
            (base == 16 && !anyHexDigitsNoSingleUS))
            error(scanloc, "`%.*s` isn't a valid integer literal, use `%.*s0` instead", cast(int)(p - start), start, 2, start);

        t.unsvalue = n;

        if (Ccompile)
            return cnumber(base, n);

        enum FLAGS : int
        {
            none = 0,
            decimal = 1, // decimal
            unsigned = 2, // u or U suffix
            long_ = 4, // L suffix
        }

        FLAGS flags = (base == 10) ? FLAGS.decimal : FLAGS.none;
        // Parse trailing 'u', 'U', 'l' or 'L' in any combination
        const psuffix = p;
        while (1)
        {
            FLAGS f;
            switch (*p)
            {
            case 'U':
            case 'u':
                f = FLAGS.unsigned;
                goto L1;
            case 'l':
                f = FLAGS.long_;
                error(scanloc, "lower case integer suffix 'l' is not allowed. Please use 'L' instead");
                goto L1;
            case 'L':
                f = FLAGS.long_;
            L1:
                p++;
                if ((flags & f) && !err)
                {
                    error(scanloc, "repeated integer suffix `%c`", p[-1]);
                    err = true;
                }
                flags = cast(FLAGS)(flags | f);
                continue;
            default:
                break;
            }
            break;
        }
        if (base == 8 && n >= 8)
        {
            if (err)
                // can't translate invalid octal value, just show a generic message
                error(scanloc, "octal literals larger than 7 are no longer supported");
            else
                error(scanloc, "octal literals `0%llo%.*s` are no longer supported, use `std.conv.octal!\"%llo%.*s\"` instead",
                    n, cast(int)(p - psuffix), psuffix, n, cast(int)(p - psuffix), psuffix);
        }
        TOK result;
        switch (flags)
        {
        case FLAGS.none:
            /* Octal or Hexadecimal constant.
             * First that fits: int, uint, long, ulong
             */
            if (n & 0x8000000000000000L)
                result = TOK.uns64Literal;
            else if (n & 0xFFFFFFFF00000000L)
                result = TOK.int64Literal;
            else if (n & 0x80000000)
                result = TOK.uns32Literal;
            else
                result = TOK.int32Literal;
            break;
        case FLAGS.decimal:
            /* First that fits: int, long, long long
             */
            if (n & 0x8000000000000000L)
            {
                result = TOK.uns64Literal;
            }
            else if (n & 0xFFFFFFFF80000000L)
                result = TOK.int64Literal;
            else
                result = TOK.int32Literal;
            break;
        case FLAGS.unsigned:
        case FLAGS.decimal | FLAGS.unsigned:
            /* First that fits: uint, ulong
             */
            if (n & 0xFFFFFFFF00000000L)
                result = TOK.uns64Literal;
            else
                result = TOK.uns32Literal;
            break;
        case FLAGS.decimal | FLAGS.long_:
            if (n & 0x8000000000000000L)
            {
                if (!err)
                {
                    error("signed integer overflow");
                    err = true;
                }
                result = TOK.uns64Literal;
            }
            else
                result = TOK.int64Literal;
            break;
        case FLAGS.long_:
            if (n & 0x8000000000000000L)
                result = TOK.uns64Literal;
            else
                result = TOK.int64Literal;
            break;
        case FLAGS.unsigned | FLAGS.long_:
        case FLAGS.decimal | FLAGS.unsigned | FLAGS.long_:
            result = TOK.uns64Literal;
            break;
        default:
            debug
            {
                printf("%x\n", flags);
            }
            assert(0);
        }
        return result;
    }

    /**************************************
     * Lex C integer-suffix
     * Params:
     *  base = number base
     *  n = raw integer value
     * Returns:
     *  token value
     */
    private TOK cnumber(int base, ulong n)
    {
        /* C11 6.4.4.1
         * Parse trailing suffixes:
         *   u or U
         *   l or L
         *   ll or LL
         */
        enum FLAGS : uint
        {
            octalhex = 1, // octal or hexadecimal
            decimal  = 2, // decimal
            unsigned = 4, // u or U suffix
            long_    = 8, // l or L suffix
            llong    = 0x10, // ll or LL

            // Microsoft extensions
            i8       = 0x20,
            i16      = 0x40,
            i32      = 0x80,
            i64      = 0x100,
        }
        FLAGS flags = (base == 10) ? FLAGS.decimal : FLAGS.octalhex;
        bool err;
    Lsuffixes:
        while (1)
        {
            FLAGS f;
            const cs = *p;
            switch (cs)
            {
                case 'U':
                case 'u':
                    f = FLAGS.unsigned;
                    break;

                case 'l':
                case 'L':
                    f = FLAGS.long_;
                    if (cs == p[1])
                    {
                        f = FLAGS.long_ | FLAGS.llong;
                        ++p;
                    }
                    break;

                case 'i':
                case 'I':
                    if (p[1] == '8')
                    {
                        f = FLAGS.i8;
                        ++p;
                    }
                    else if (p[1] == '1' && p[2] == '6')
                    {
                        f = FLAGS.i16;
                        p += 2;
                    }
                    else if (p[1] == '3' && p[2] == '2')
                    {
                        f = FLAGS.i32;
                        p += 2;
                    }
                    else if (p[1] == '6' && p[2] == '4')
                    {
                        f = FLAGS.i64;
                        p += 2;
                    }
                    else
                        break Lsuffixes;
                    if (p[1] >= '0' && p[1] <= '9' && !err)
                    {
                        error("invalid integer suffix");
                        err = true;
                    }
                    break;

                default:
                    break Lsuffixes;
            }
            ++p;
            if ((flags & f) && !err)
            {
                error("duplicate integer suffixes");
                err = true;
            }
            flags = cast(FLAGS)(flags | f);
        }

        TOK result = TOK.int32Literal;     // default
        switch (flags)
        {
            /* Since D doesn't have a variable sized `long` or `unsigned long` type,
             * this code deviates from C by picking D int, uint, long, or ulong instead
             */

            case FLAGS.octalhex:
                /* Octal or Hexadecimal constant.
                 * First that fits: int, unsigned, long, unsigned long,
                 * long long, unsigned long long
                 */
                if (n & 0x8000000000000000L)
                    result = TOK.uns64Literal;      // unsigned long
                else if (n & 0xFFFFFFFF00000000L)
                    result = TOK.int64Literal;      // long
                else if (n & 0x80000000)
                    result = TOK.uns32Literal;
                else
                    result = TOK.int32Literal;
                break;

            case FLAGS.decimal:
                /* First that fits: int, long, long long
                 */
                if (n & 0x8000000000000000L)
                    result = TOK.uns64Literal;      // unsigned long
                else if (n & 0xFFFFFFFF80000000L)
                    result = TOK.int64Literal;      // long
                else
                    result = TOK.int32Literal;
                break;

            case FLAGS.octalhex | FLAGS.unsigned:
            case FLAGS.decimal | FLAGS.unsigned:
                /* First that fits: unsigned, unsigned long, unsigned long long
                 */
                if (n & 0xFFFFFFFF00000000L)
                    result = TOK.uns64Literal;      // unsigned long
                else
                    result = TOK.uns32Literal;
                break;

            case FLAGS.decimal | FLAGS.long_:
                /* First that fits: long, long long
                 */
                if (longsize == 4 || long_longsize == 4)
                {
                    if (n & 0xFFFFFFFF_80000000L)
                        result = TOK.int64Literal;
                    else
                        result = TOK.int32Literal;  // long
                }
                else
                {
                    result = TOK.int64Literal;      // long
                }
                break;

            case FLAGS.octalhex | FLAGS.long_:
                /* First that fits: long, unsigned long, long long,
                 * unsigned long long
                 */
                if (longsize == 4 || long_longsize == 4)
                {
                    if (n & 0x8000000000000000L)
                        result = TOK.uns64Literal;
                    else if (n & 0xFFFFFFFF00000000L)
                        result = TOK.int64Literal;
                    else if (n & 0x80000000)
                        result = TOK.uns32Literal;      // unsigned long
                    else
                        result = TOK.int32Literal;      // long
                }
                else
                {
                    if (n & 0x80000000_00000000L)
                        result = TOK.uns64Literal;      // unsigned long
                    else
                        result = TOK.int64Literal;      // long
                }
                break;

            case FLAGS.octalhex | FLAGS.unsigned | FLAGS.long_:
            case FLAGS.decimal  | FLAGS.unsigned | FLAGS.long_:
                /* First that fits: unsigned long, unsigned long long
                 */
                if (longsize == 4 || long_longsize == 4)
                {
                    if (n & 0xFFFFFFFF00000000L)
                        result = TOK.uns64Literal;
                    else
                        result = TOK.uns32Literal;      // unsigned long
                }
                else
                {
                    result = TOK.uns64Literal;  // unsigned long
                }
                break;

            case FLAGS.octalhex | FLAGS.long_ | FLAGS.llong:
                /* First that fits: long long, unsigned long long
                 */
                if (n & 0x8000000000000000L)
                    result = TOK.uns64Literal;
                else
                    result = TOK.int64Literal;
                break;

            case FLAGS.decimal | FLAGS.long_ | FLAGS.llong:
                /* long long
                 */
                result = TOK.int64Literal;
                break;

            case FLAGS.octalhex | FLAGS.long_ | FLAGS.unsigned | FLAGS.llong:
            case FLAGS.decimal  | FLAGS.long_ | FLAGS.unsigned | FLAGS.llong:
                result = TOK.uns64Literal;
                break;

            case FLAGS.octalhex | FLAGS.i8:
            case FLAGS.octalhex | FLAGS.i16:
            case FLAGS.octalhex | FLAGS.i32:
            case FLAGS.octalhex | FLAGS.unsigned | FLAGS.i8:
            case FLAGS.octalhex | FLAGS.unsigned | FLAGS.i16:
            case FLAGS.octalhex | FLAGS.unsigned | FLAGS.i32:
            case FLAGS.decimal  | FLAGS.unsigned | FLAGS.i8:
            case FLAGS.decimal  | FLAGS.unsigned | FLAGS.i16:
            case FLAGS.decimal  | FLAGS.unsigned | FLAGS.i32:
                result = TOK.uns32Literal;
                break;

            case FLAGS.decimal | FLAGS.i8:
            case FLAGS.decimal | FLAGS.i16:
            case FLAGS.decimal | FLAGS.i32:
                result = TOK.int32Literal;
                break;

            case FLAGS.octalhex | FLAGS.i64:
            case FLAGS.octalhex | FLAGS.unsigned | FLAGS.i64:
            case FLAGS.decimal  | FLAGS.unsigned | FLAGS.i64:
                result = TOK.uns64Literal;
                break;

            case FLAGS.decimal | FLAGS.i64:
                result = TOK.int64Literal;
                break;

            default:
                debug printf("%x\n",flags);
                assert(0);
        }
        return result;
    }

    /**************************************
     * Read in characters, converting them to real.
     * Bugs:
     *      Exponent overflow not detected.
     *      Too much requested precision is not detected.
     */
    private TOK inreal(Token* t)
    {
        //printf("Lexer::inreal()\n");
        debug
        {
            assert(*p == '.' || isdigit(*p));
        }
        bool isWellformedString = true;
        stringbuffer.setsize(0);
        auto pstart = p;
        bool hex = false;
        dchar c = *p++;
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
                    if (Ccompile)
                        error("embedded `_` in numeric literals not allowed");
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
        auto sbufptr = cast(const(char)*)stringbuffer[].ptr;
        TOK result;
        bool isOutOfRange = false;
        t.floatvalue = (isWellformedString ? CTFloat.parse(sbufptr, isOutOfRange) : CTFloat.zero);

        bool imaginary = false;
        if (*p == 'i' && Ccompile)
        {
            ++p;
            imaginary = true;
        }

        switch (*p)
        {
        case 'F':
        case 'f':
            if (isWellformedString && !isOutOfRange)
                isOutOfRange = Port.isFloat32LiteralOutOfRange(sbufptr);
            result = TOK.float32Literal;
            p++;
            break;
        default:
            if (isWellformedString && !isOutOfRange)
                isOutOfRange = Port.isFloat64LiteralOutOfRange(sbufptr);
            result = TOK.float64Literal;
            break;
        case 'l':
            if (!Ccompile)
                error("use 'L' suffix instead of 'l'");
            goto case 'L';
        case 'L':
            ++p;
            if (Ccompile && long_doublesize == 8)
                goto default;
            result = TOK.float80Literal;
            break;
        }

        if ((*p == 'i' || *p == 'I') && !Ccompile)
        {
            if (*p == 'I')
                error("use 'i' suffix instead of 'I'");
            p++;
            imaginary = true;
        }

        if (imaginary)
        {
            switch (result)
            {
            case TOK.float32Literal:
                result = TOK.imaginary32Literal;
                break;
            case TOK.float64Literal:
                result = TOK.imaginary64Literal;
                break;
            case TOK.float80Literal:
                result = TOK.imaginary80Literal;
                break;
            default:
                break;
            }
        }
        const isLong = (result == TOK.float80Literal || result == TOK.imaginary80Literal);
        if (isOutOfRange && !isLong && (!Ccompile || hex))
        {
            /* C11 6.4.4.2 doesn't actually care if it is not representable if it is not hex
             */
            const char* suffix = result == TOK.float32Literal ? "f" : result == TOK.float80Literal ? "L" : "";
            const char* type = [TOK.float32Literal: "`float`".ptr,
                                TOK.float64Literal: "`double`".ptr,
                                TOK.float80Literal: "`real` for the current target".ptr][result];
            error(scanloc, "number `%s%s` is not representable as a %s", sbufptr, suffix, type);
            const char* extra = result == TOK.float64Literal ? "`real` literals can be written using the `L` suffix. " : "";
            eSink.errorSupplemental(scanloc, "%shttps://dlang.org/spec/lex.html#floatliteral", extra);
        }
        debug
        {
            switch (result)
            {
            case TOK.float32Literal:
            case TOK.float64Literal:
            case TOK.float80Literal:
            case TOK.imaginary32Literal:
            case TOK.imaginary64Literal:
            case TOK.imaginary80Literal:
                break;
            default:
                assert(0);
            }
        }
        return result;
    }

    final Loc loc() @nogc
    {
        scanloc = baseLoc.getLoc(cast(uint) (p - base));
        return scanloc;
    }

    void error(T...)(const(char)* format, T args)
    {
        eSink.error(token.loc, format, args);
    }

    void error(T...)(Loc loc, const(char)* format, T args)
    {
        eSink.error(loc, format, args);
    }

    void errorSupplemental(T...)(const(char)* format, T args)
    {
        eSink.errorSupplemental(token.loc, format, args);
    }

    void deprecation(T...)(Loc loc, const(char)* format, T args)
    {
        eSink.deprecation(loc, format, args);
    }

    void warning(T...)(Loc loc, const(char)* format, T args)
    {
        eSink.warning(loc, format, args);
    }

    void deprecation(T...)(const(char)* format, T args)
    {
        eSink.deprecation(token.loc, format, args);
    }

    void deprecationSupplemental(T...)(const(char)* format, T args)
    {
        eSink.deprecationSupplemental(token.loc, format, args);
    }

    /***************************************
     * Parse special token sequence:
     * Returns:
     *  true if the special token sequence was handled
     * References:
     *  https://dlang.org/spec/lex.html#special-token-sequence
     */
    bool parseSpecialTokenSequence()
    {
        Token n;
        scan(&n);
        if (n.value == TOK.identifier)
        {
            if (n.ident == Id.line)
            {
                poundLine(n, false);
                return true;
            }
            else
            {
                const locx = loc();
                // @@@DEPRECATED_2.103@@@
                // Turn into an error in 2.113
                if (inTokenStringConstant)
                    deprecation(locx, "token string requires valid D tokens, not `#%s`", n.ident.toChars());
                else
                    error(locx, "C preprocessor directive `#%s` is not supported", n.ident.toChars());
            }
        }
        else if (n.value == TOK.if_)
        {
            const locx = loc();
            if (inTokenStringConstant)
                error(locx, "token string requires valid D tokens, not `#if`");
            else
                error(locx, "C preprocessor directive `#if` is not supported, use `version` or `static if`");
        }
        return false;
    }

    /*********************************************
     * Parse line/file preprocessor directive:
     *    #line linnum [filespec]
     * Allow __LINE__ for linnum, and __FILE__ for filespec.
     * Accept linemarker format:
     *    # linnum [filespec] {flags}
     * There can be zero or more flags, which are one of the digits 1..4, and
     * must be in ascending order. The flags are ignored.
     * Params:
     *  tok = token we're on, which is linnum of linemarker
     *  linemarker = true if line marker format and lexer is on linnum
     * References:
     *  linemarker https://gcc.gnu.org/onlinedocs/gcc-11.1.0/cpp/Preprocessor-Output.html
     */
    final void poundLine(ref Token tok, bool linemarker)
    {
        const(char)* filespec = null;
        bool flags;

        if (!linemarker)
            scan(&tok);
        if (tok.value == TOK.int32Literal || tok.value == TOK.int64Literal)
        {
            const lin = cast(int)(tok.unsvalue);
            if (lin != tok.unsvalue)
            {
                error(tok.loc, "line number `%lld` out of range", cast(ulong)tok.unsvalue);
                skipToNextLine();
                return;
            }
            else
                linnum = lin;
        }
        else if (tok.value == TOK.line)  // #line __LINE__
        {
        }
        else
        {
            error(tok.loc, "positive integer argument expected following `#line`");
            if (tok.value != TOK.endOfLine)
                skipToNextLine();
            return;
        }
        while (1)
        {
            scan(&tok);
            switch (tok.value)
            {
            case TOK.endOfFile:
            case TOK.endOfLine:
                if (!inTokenStringConstant)
                {
                    baseLoc.addSubstitution(cast(uint) (p - base), filespec, linnum);
                }
                return;
            case TOK.file:
                if (filespec || flags)
                    goto Lerr;
                filespec = mem.xstrdup(scanloc.filename);
                continue;
            case TOK.string_:
                if (filespec || flags)
                    goto Lerr;
                if (tok.ptr[0] != '"' || tok.postfix != 0)
                    goto Lerr;
                filespec = tok.ustring;
                continue;
            case TOK.int32Literal:
                if (!filespec)
                    goto Lerr;
                if (linemarker && tok.unsvalue >= 1 && tok.unsvalue <= 4)
                {
                    flags = true;   // linemarker flags seen
                    continue;
                }
                goto Lerr;
            default:
                goto Lerr;
            }
        }
    Lerr:
        if (filespec is null)
            error(tok.loc, "invalid filename for `#line` directive");
        else if (linemarker)
            error(tok.loc, "invalid flag for line marker directive");
        else if (!Ccompile)
            error(tok.loc, "found `%s` when expecting new line following `#line` directive", tok.toChars());
        if (tok.value != TOK.endOfLine)
            skipToNextLine();
    }

    /***************************************
     * Scan forward to start of next line.
     * Params:
     *    defines = send characters to `defines`
     */
    final void skipToNextLine(OutBuffer* defines = null)
    {
        while (1)
        {
            switch (*p)
            {
            case 0:
            case 0x1A:
                return; // do not advance p

            case '\n':
                ++p;
                break;

            case '\r':
                ++p;
                if (p[0] == '\n')
                   ++p;
                break;

            default:
                if (defines)
                    defines.writeByte(*p); // don't care about Unicode line endings for C
                else if (*p & 0x80)
                {
                    const u = decodeUTF();
                    if (u == PS || u == LS)
                    {
                        ++p;
                        break;
                    }
                }
                ++p;
                continue;
            }
            break;
        }
        endOfLine();
        tokenizeNewlines = false;
    }

    /********************************************
     * Decode UTF character.
     * Issue error messages for invalid sequences.
     * Return decoded character, advance p to last character in UTF sequence.
     */
    private uint decodeUTF()
    {
        string msg;
        auto result = decodeUTFpure(msg);

        if (msg)
            error(token.loc, "%.*s", cast(int)msg.length, msg.ptr);
        return result;
    }

    /********************************************
     * Same as above, but the potential error message is stored to the
     * msg parameter instead of being issued.
     */
    private pure uint decodeUTFpure(out string msg)
    {
        const s = p;
        assert(*s & 0x80);
        // Check length of remaining string up to 4 UTF-8 characters
        size_t len;
        for (len = 1; len < 4 && s[len]; len++)
        {
        }
        size_t idx = 0;
        dchar u;
        msg = utf_decodeChar(s[0 .. len], idx, u);
        p += idx - 1;
        if (!msg && isBidiControl(u))
            msg = "Bidirectional control characters are disallowed for security reasons.";
        return u;
    }

    /***************************************************
     * Parse doc comment embedded between t.ptr and p.
     * Remove trailing blanks and tabs from lines.
     * Replace all newlines with \n.
     * Remove leading comment character from each line.
     * Decide if it's a lineComment or a blockComment.
     * Append to previous one for this token.
     *
     * If newParagraph is true, an extra newline will be
     * added between adjoining doc comments.
     */
    private void getDocComment(Token* t, uint lineComment, bool newParagraph) pure
    {
        /* ct tells us which kind of comment it is: '/', '*', or '+'
         */
        const ct = t.ptr[2];
        /* Start of comment text skips over / * *, / + +, or / / /
         */
        const(char)* q = t.ptr + 3; // start of comment text
        const(char)* qend = p;
        if (ct == '*' || ct == '+')
            qend -= 2;
        /* Scan over initial row of ****'s or ++++'s or ////'s
         */
        for (; q < qend; q++)
        {
            if (*q != ct)
                break;
        }
        /* Remove leading line feed or space
         */
        int linestart = 0;
        if (ct == '/')
        {
            if (q < qend && *q == ' ')
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

        void trimTrailingWhitespace()
        {
            const s = buf[];
            auto len = s.length;
            while (len && (s[len - 1] == ' ' || s[len - 1] == '\t'))
                --len;
            buf.setsize(len);
        }

        for (; q < qend; q++)
        {
            char c = *q;
            switch (c)
            {
            case '*':
            case '+':
                if (linestart && c == ct)
                {
                    linestart = 0;
                    /* Trim preceding whitespace up to preceding \n
                     */
                    trimTrailingWhitespace();
                    continue;
                }
                break;
            case ' ':
            case '\t':
                break;
            case '\r':
                if (q[1] == '\n')
                    continue; // skip the \r
                goto Lnewline;
            default:
                if (c == 226)
                {
                    // If LS or PS
                    if (q[1] == 128 && (q[2] == 168 || q[2] == 169))
                    {
                        q += 2;
                        goto Lnewline;
                    }
                }
                linestart = 0;
                break;
            Lnewline:
                c = '\n'; // replace all newlines with \n
                goto case;
            case '\n':
                linestart = 1;
                /* Trim trailing whitespace
                 */
                trimTrailingWhitespace();
                break;
            }
            buf.writeByte(c);
        }
        /* Trim trailing whitespace (if the last line does not have newline)
         */
        trimTrailingWhitespace();

        // Always end with a newline
        const s = buf[];
        if (s.length == 0 || s[$ - 1] != '\n')
            buf.writeByte('\n');

        // It's a line comment if the start of the doc comment comes
        // after other non-whitespace on the same line.
        auto dc = (lineComment && anyToken) ? &t.lineComment : &t.blockComment;
        // Combine with previous doc comment, if any
        if (*dc)
        {
            auto p = combineComments(*dc, buf[], newParagraph);
            *dc = p ? p[0 .. strlen(p)] : null;
        }
        else
            *dc = buf.extractSlice(true);
    }

    /********************************************
     * Combine two document comments into one,
     * separated by an extra newline if newParagraph is true.
     */
    static const(char)* combineComments(const(char)[] c1, const(char)[] c2, bool newParagraph) pure
    {
        //debug printf("Lexer::combineComments('%*.s', '%*.s', '%i')\n", cast(int) c1.length, c1.ptr, cast(int) c2.length, c2.ptr, newParagraph);
        const(int) newParagraphSize = newParagraph ? 1 : 0; // Size of the combining '\n'
        if (!c1)
            return c2.ptr;
        if (!c2)
            return c1.ptr;

        int insertNewLine = 0;
        if (c1.length && c1[$ - 1] != '\n')
            insertNewLine = 1;
        const retSize = c1.length + insertNewLine + newParagraphSize + c2.length;
        auto p = cast(char*)mem.xmalloc_noscan(retSize + 1);
        p[0 .. c1.length] = c1[];
        if (insertNewLine)
            p[c1.length] = '\n';
        if (newParagraph)
            p[c1.length + insertNewLine] = '\n';
        p[retSize - c2.length .. retSize] = c2[];
        p[retSize] = 0;
        return p;
    }

    /**************************
     * `p` should be at start of next line
     */
    private void endOfLine() @safe
    {
        linnum += 1;
        line = p;
        baseLoc.newLine(cast(uint)(p - base));
    }

    /****************************
     * Print the tokens from the current `token` to the end,
     * while not advancing the parser forward.
     * Useful for debugging.
     */
    void printRestOfTokens()
    {
        auto tk = &token;
        while (1)
        {
            printf("%s ", (*tk).toChars());
            if (tk.value == TOK.endOfFile || tk.value == TOK.endOfLine)
                break;
            tk = peek(tk);
        }
        printf("\n");
    }
}

/******************************* Unittest *****************************************/

unittest
{
    fprintf(stderr, "Lexer.unittest %d\n", __LINE__);

    ErrorSink errorSink = new ErrorSinkStderr;

    void test(T)(string sequence, T expected, bool Ccompile = false)
    {
        auto p = cast(const(char)*)sequence.ptr;
        dchar c2;
        Lexer lexer = new Lexer(errorSink);
        assert(expected == lexer.escapeSequence(Loc.initial, p, Ccompile, c2));
        assert(p == sequence.ptr + sequence.length);
    }

    test(`'`, '\'');
    test(`"`, '"');
    test(`?`, '?');
    test(`\`, '\\');
    test(`0`, '\0');
    test(`a`, '\a');
    test(`b`, '\b');
    test(`f`, '\f');
    test(`n`, '\n');
    test(`r`, '\r');
    test(`t`, '\t');
    test(`v`, '\v');

    test(`x00`, 0x00);
    test(`xff`, 0xff);
    test(`xFF`, 0xff);
    test(`xa7`, 0xa7);
    test(`x3c`, 0x3c);
    test(`xe2`, 0xe2);

    test(`1`, '\1');
    test(`42`, '\42');
    test(`357`, '\357');

    test(`u1234`, '\u1234');
    test(`uf0e4`, '\uf0e4');

    test(`U0001f603`, '\U0001f603');

    test(`&quot;`, '"');
    test(`&lt;`, '<');
    test(`&gt;`, '>');
}

unittest
{
    fprintf(stderr, "Lexer.unittest %d\n", __LINE__);

    static class ErrorSinkTest : ErrorSinkNull
    {
      nothrow:
      extern (C++):
      override:

        import core.stdc.stdio;
        import core.stdc.stdarg;

        string expected;
        string expectedSupplemental;
        bool gotError;

        void verror(Loc loc, const(char)* format, va_list ap)
        {
            gotError = true;
            char[100] buffer = void;
            auto actual = buffer[0 .. vsnprintf(buffer.ptr, buffer.length, format, ap)];
            assert(expected == actual);
        }

        void errorSupplemental(Loc loc, const(char)* format, ...)
        {
            gotError = true;
            char[128] buffer = void;
            va_list ap;
            va_start(ap, format);
            auto actual = buffer[0 .. vsnprintf(buffer.ptr, buffer.length, format, ap)];
            va_end(ap);
            assert(expectedSupplemental == actual);
        }
    }

    ErrorSinkTest errorSink = new ErrorSinkTest;

    void test2(string sequence, string[2] expectedError, dchar expectedReturnValue, uint expectedScanLength, bool Ccompile = false)
    {
        errorSink.expected = expectedError[0];
        errorSink.expectedSupplemental = expectedError[1];
        errorSink.gotError = false;
        auto p = cast(const(char)*)sequence.ptr;
        Lexer lexer = new Lexer(errorSink);
        dchar c2;
        auto actualReturnValue = lexer.escapeSequence(Loc.initial, p, Ccompile, c2);
        assert(errorSink.gotError);
        assert(expectedReturnValue == actualReturnValue);

        auto actualScanLength = p - sequence.ptr;
        assert(expectedScanLength == actualScanLength);
    }

    void test(string sequence, string expectedError, dchar expectedReturnValue, uint expectedScanLength, bool Ccompile = false)
    {
        test2(sequence, [expectedError, null], expectedReturnValue, expectedScanLength, Ccompile);
    }

    test("c", `undefined escape sequence \c`, 'c', 1);
    test("!", `undefined escape sequence \!`, '!', 1);
    test("&quot;", `undefined escape sequence \&`, '&', 1, true);

    test("x1", `escape hex sequence has 1 hex digits instead of 2`, '\x01', 2);

    test("u1"  , `escape hex sequence has 1 hex digits instead of 4`,   0x1, 2);
    test("u12" , `escape hex sequence has 2 hex digits instead of 4`,  0x12, 3);
    test("u123", `escape hex sequence has 3 hex digits instead of 4`, 0x123, 4);

    test("U0"      , `escape hex sequence has 1 hex digits instead of 8`,       0x0, 2);
    test("U00"     , `escape hex sequence has 2 hex digits instead of 8`,      0x00, 3);
    test("U000"    , `escape hex sequence has 3 hex digits instead of 8`,     0x000, 4);
    test("U0000"   , `escape hex sequence has 4 hex digits instead of 8`,    0x0000, 5);
    test("U0001f"  , `escape hex sequence has 5 hex digits instead of 8`,   0x0001f, 6);
    test("U0001f6" , `escape hex sequence has 6 hex digits instead of 8`,  0x0001f6, 7);
    test("U0001f60", `escape hex sequence has 7 hex digits instead of 8`, 0x0001f60, 8);

    test("U00110000", `invalid UTF character \U00110000`, '?', 9);

    test("xg0"      , `undefined escape hex sequence \xg`, 'g', 2);
    test("ug000"    , `undefined escape hex sequence \ug`, 'g', 2);
    test("Ug0000000", `undefined escape hex sequence \Ug`, 'g', 2);

    test("&BAD;", `unnamed character entity &BAD;`  , '?', 5);
    test("&quot", `unterminated named entity &quot;`, '?', 5);
    test("&quot", `unterminated named entity &quot;`, '?', 5);

    test("400", `escape octal sequence \400 is larger than \377`, 0x100, 3);

    test2("uD800", [`invalid UTF character \U0000d800`, `The code unit is a UTF-16 surrogate, is the escape UTF-16 not a Unicode code point?`], '?', 5);
    test2("uDFFF", [`invalid UTF character \U0000dfff`, `The code unit is a UTF-16 surrogate, is the escape UTF-16 not a Unicode code point?`], '?', 5);
}

unittest
{
    fprintf(stderr, "Lexer.unittest %d\n", __LINE__);
    /* Not much here, just trying things out.
     */
    string text = "int"; // We rely on the implicit null-terminator
    ErrorSink errorSink = new ErrorSinkStderr;
    scope Lexer lex1 = new Lexer(null, text.ptr, 0, text.length, false, false, errorSink, null);
    TOK tok;
    tok = lex1.nextToken();
    //printf("tok == %s, %d, %d\n", Token::toChars(tok), tok, TOK.int32);
    assert(tok == TOK.int32);
    tok = lex1.nextToken();
    assert(tok == TOK.endOfFile);
    tok = lex1.nextToken();
    assert(tok == TOK.endOfFile);
    tok = lex1.nextToken();
    assert(tok == TOK.endOfFile);
}

unittest
{
    fprintf(stderr, "Lexer.unittest %d\n", __LINE__);

    // We don't want to see Lexer error output during these tests.
    ErrorSink errorSink = new ErrorSinkNull;

    // Test malformed input: even malformed input should end in a TOK.endOfFile.
    static immutable char[][] testcases =
    [   // Testcase must end with 0 or 0x1A.
        [0], // not malformed, but pathological
        ['\'', 0],
        ['\'', 0x1A],
        ['{', '{', 'q', '{', 0],
        [0xFF, 0],
        [0xFF, 0x80, 0],
        [0xFF, 0xFF, 0],
        [0xFF, 0xFF, 0],
        ['x', '"', 0x1A],
    ];

    foreach (testcase; testcases)
    {
        scope Lexer lex2 = new Lexer(null, testcase.ptr, 0, testcase.length-1, false, false, errorSink, null);
        TOK tok = lex2.nextToken();
        size_t iterations = 1;
        while ((tok != TOK.endOfFile) && (iterations++ < testcase.length))
        {
            tok = lex2.nextToken();
        }
        assert(tok == TOK.endOfFile);
        tok = lex2.nextToken();
        assert(tok == TOK.endOfFile);
    }
}
