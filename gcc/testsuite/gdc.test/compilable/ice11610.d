
struct Token
{
    TokenType type;
}

enum TokenType : ushort
{
    invalid
}

class Parser
{
    bool peekIsOneOf(TokenType[] types...)
    {
        canFind(types, tokens[1].type);
        return true;
    }

    Token[] tokens;
}

/*************************************************/
// std.algorithm

R find(alias pred = "a == b", R, E)(R haystack, E needle)
{
    enum isIntegralNeedle = isSomeChar!E/* || isIntegral!E || isBoolean!E*/;

    return haystack;
}

bool canFind(alias pred = "a == b", R, E)(R haystack, E needle)
if (is(typeof(find!pred(haystack, needle))))        // 1st instantiate of find template with error gagging
{
    return find!pred(haystack, needle).length != 0; // 2nd instantiate of find template without gagging
}

/*************************************************/
// std.traits

template CharTypeOf(T)
{
           inout( char) idx(        inout( char) );
           inout(wchar) idx(        inout(wchar) );
           inout(dchar) idx(        inout(dchar) );
    shared(inout  char) idx( shared(inout  char) );
    shared(inout wchar) idx( shared(inout wchar) );
    shared(inout dchar) idx( shared(inout dchar) );

    static if (is(T == enum))
    {
        /* This line instantiates CharTypeOf!short and will make error.
         * But, when CharTypeOf!short is re-instantiated without gagging,
         * that's for correct error report, its 'members' does not re-created.
         * so, members' semantic will call FuncDeclaration::overloadInsert of
         * 'idx' functions, and will make circular linked list of
         * FuncDeclaration::overnext. Finally, iterating it will cause
         * infinite recursion and compiler segfault.
         */
        alias .CharTypeOf!(OriginalType!T) CharTypeOf;
    }
    else static if (is(typeof(idx(T.init)) X))
    {
        alias X CharTypeOf;
    }
    else
        static assert(0, T.stringof~" is not a character type");
}

template isSomeChar(T)
{
    enum isSomeChar = is(CharTypeOf!T);
}

template OriginalType(T)
{
    alias OriginalType = ushort;
}
