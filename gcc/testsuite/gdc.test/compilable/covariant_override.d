// https://issues.dlang.org/show_bug.cgi?id=21538
// REQUIRED_ARGS: -preview=dip1000

interface I
{
    void f(void delegate() @safe dg) @safe;
}

class CI : I
{
    override void f(void delegate() @system dg) @safe { }
}

abstract class A
{
    void f(void delegate() @safe dg) @safe;
}

class CA : A
{
    override void f(void delegate() @system dg) @safe { }
}

// https://issues.dlang.org/show_bug.cgi?id=20904
auto blah(void delegate())
{
}

void delegate()[string] r;
void main()
{
    void delegate() nothrow a;
    r["v"] = a;
}
