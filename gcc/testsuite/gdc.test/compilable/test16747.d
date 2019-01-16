/*
PERMUTE_ARGS:
*/

class C { @safe ~this() { } }
class D : C { }

void fwd() @safe
{
    scope o = new Object();
    scope c = new C();
    scope d = new D();
}
