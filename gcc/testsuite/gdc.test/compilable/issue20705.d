// REQUIRED_ARGS: -preview=rvaluerefparam
struct Foo
{
    int[] a;
}

void bar (T) (const ref T arg) {}
T foo (T) (ref T arg) { return arg; }
void goo()(ref long x) { x = 1; }
void main ()
{
    bar(Foo([42]));
    auto x = foo(Foo([42]));
    int y;
    static assert(!__traits(compiles, goo(y)));
}
