/*
TEST_OUTPUT:
---
Hello
World
from
CTFE
Hello[1 .. 3] = el
S.str
abcdefghij[2 .. 6] = cdef
---
*/

struct S
{
    string str = "S.str";
    alias str this;
}

int greeting(scope const char[][] values) pure nothrow @safe @nogc
{
    const string newline = "\n";

    foreach (const val; values)
    {
        __ctfeWrite(val);
        __ctfeWrite(newline);
    }

    // Test slices
    const val = values[0];
    __ctfeWrite(val[]);
    __ctfeWrite(['[','1',' ','.','.',' ','3',']',' ','=',' ']);
    __ctfeWrite(val[1 .. 3]);
    __ctfeWrite(newline);

    S s;
    __ctfeWrite(s);
    __ctfeWrite(newline);

    // Test mutable slices
    char[10] buffer;
    fill(buffer); // Avoid potential shortcuts for literals
    __ctfeWrite(buffer[0 .. $]);
    __ctfeWrite("[2 .. 6] = ");
    __ctfeWrite(buffer[2 .. 6]);
    __ctfeWrite(newline);

    return 0;
}

void fill(ref char[10] buffer) pure nothrow @safe @nogc
{
    foreach (const idx, ref ch; buffer)
        ch = cast(char)('a' + idx);
}

enum forceCTFE = greeting(["Hello", "World", "from", "CTFE"]);
