// https://issues.dlang.org/show_bug.cgi?id=15019
// dmd -m32 -c all.d

import std.string;

struct Color()
{
    static fromHex(char[] s)
    {
        import std.conv;
        s.to!ubyte;
    }
}

Color!() RGB    ;

struct Matrix(T, int R, int C)
{
        Vector!(T, C) row_t;
        T[C] v;        // all elements

        /// Covnerts to pretty string.
        string toString() const
        {
            try
                return format("%s", v);
            catch
                assert(false); // should not happen since format is right
        }
}

// GLSL is a big inspiration here
// we defines types with more or less the same names
template mat2x2(T) { Matrix!(T, 2, 2) mat2x2; }
template mat3x3(T) { Matrix!(T, 3, 3) mat3x3; }
template mat4x4(T) { Matrix!(T, 4, 4) mat4x4; }

alias mat2x2 mat2;
alias mat3x3 mat3;  // shorter names for most common matrices
alias mat4x4 mat4;

string definePostfixAliases(string type)
{
    return "alias " ~ type ~ "!byte "   ~ type ~ "b;\n"
~ "alias " ~ type ~ "!ubyte "  ~ type ~ "ub;\n"
~ "alias " ~ type ~ "!short "  ~ type ~ "s;\n"
~ "alias " ~ type ~ "!ushort " ~ type ~ "us;\n"
~ "alias " ~ type ~ "!int "    ~ type ~ "i;\n"
~ "alias " ~ type ~ "!uint "   ~ type ~ "ui;\n"
~ "alias " ~ type ~ "!long "   ~ type ~ "l;\n"
~ "alias " ~ type ~ "!ulong "  ~ type ~ "ul;\n"
~ "alias " ~ type ~ "!float "  ~ type ~ "f;\n"
~ "alias " ~ type ~ "!double " ~ type ~ "d;\n";
}

// define a lot of type names
mixin(definePostfixAliases("mat2"));
mixin(definePostfixAliases("mat3"));
mixin(definePostfixAliases("mat4"));
import std.string;

struct Vector(T, int N)
{
    T[N] v;

    string toString()
    {
        try
            return format("%s", v);
        catch
            assert(false);
    }
}

