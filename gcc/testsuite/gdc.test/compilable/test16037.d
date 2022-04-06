/* REQUIRED_ARGS: -preview=dip1000
 */

// https://issues.dlang.org/show_bug.cgi?id=16037

@safe:

void testXXX () @nogc
{
    Object o;
    scope bool delegate (Object) alwaysFalse = (Object y) { return false; };
    scope c1 = o !is null ? (Object y) { return o is y; } : alwaysFalse;
}

auto f() @nogc
{
    int a;
    void g(){ a=1; }
    scope h=&g;
    h();
}
