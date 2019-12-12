/*
TEST_OUTPUT:
---
fail_compilation/fail117.d(35): Error: expression has no value
fail_compilation/fail117.d(36): Error: expression has no value
---
*/

// Issue 420 - mixin make dmd break

//import std.stdio;

template MGettor(alias Fld)
{
    typeof(Fld) opCall()
    {
        //writefln("getter");
        return Fld;
    }
}

class Foo
{
    int a = 1,
        b = 2;

    mixin MGettor!(a) geta;
    mixin MGettor!(b) getb;
}

void main()
{
    auto foo = new Foo;

    int a = foo.geta;
    int b = foo.getb;
}
