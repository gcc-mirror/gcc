/*
TEST_OUTPUT:
---
fail_compilation/fail99.d(12): Error: delegate dg (int) is not callable using argument types ()
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
