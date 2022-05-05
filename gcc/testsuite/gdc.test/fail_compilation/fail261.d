/*
TEST_OUTPUT:
---
fail_compilation/fail261.d(19): Error: invalid `foreach` aggregate `range` of type `MyRange`
fail_compilation/fail261.d(19):        maybe define `opApply()`, range primitives, or use `.tupleof`
---
*/

//import std.stdio;

struct MyRange
{
}

void main()
{
    MyRange range;

    foreach (r; range)
    {
        //writefln("%s", r.toString());
    }
}
