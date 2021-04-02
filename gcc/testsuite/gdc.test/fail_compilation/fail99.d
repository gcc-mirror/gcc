/*
TEST_OUTPUT:
---
fail_compilation/fail99.d(13): Error: delegate `dg(int)` is not callable using argument types `()`
fail_compilation/fail99.d(13):        missing argument for parameter #1: `int`
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
