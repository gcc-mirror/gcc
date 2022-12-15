/*
TEST_OUTPUT:
---
fail_compilation/fail99.d(13): Error: delegate `dg(int)` is not callable using argument types `()`
fail_compilation/fail99.d(13):        too few arguments, expected 1, got 0
---
*/

//import std.stdio;

void foo(void delegate(int) dg)
{
    dg();
    //writefln("%s", dg(3));
}

void main()
{
    foo(delegate(int i)
        {
            //writefln("i = %d\n", i);
        }
       );
}
