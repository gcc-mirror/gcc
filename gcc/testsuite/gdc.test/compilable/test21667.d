// Issue 21667 - scope parameter causes 'no size because of forward references'
// https://issues.dlang.org/show_bug.cgi?id=21667
@safe:

struct Foo
{
    void delegate(scope Foo) dg;
}

struct M
{
    F.Type f;
}

struct F
{
    enum Type {a}
    void foo(scope M m) {}
}
