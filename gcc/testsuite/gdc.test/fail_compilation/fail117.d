/*
TEST_OUTPUT:
---
fail_compilation/fail117.d(37): Error: expression `foo.mixin MGettor!(a) geta;
` is `void` and has no value
fail_compilation/fail117.d(38): Error: expression `foo.mixin MGettor!(b) getb;
` is `void` and has no value
---
*/

// https://issues.dlang.org/show_bug.cgi?id=420
// mixin make dmd break
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
