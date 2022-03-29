// https://issues.dlang.org/show_bug.cgi?id=9490
class A
{
    int[1] arr;

    this()
    {
        assert(arr.length);
        assert((arr).length);
    }
}

class C
{
    struct Foo { int a; void funcToo(){} }
    Foo foo;

    auto get(){return foo;}

    void test()
    {
        // Error: need 'this' to access member a
        (foo).a = 1;
        (foo).funcToo();
        (get()).a = 2;
    }
}

struct S { int i; }
struct S1 { S s; }
void f(int) { }

void main()
{
    S1 s1;
    f(s1.s.tupleof); // OK
    f((s1.s).tupleof); // Error: need 'this' to access member s
}
