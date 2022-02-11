// https://issues.dlang.org/show_bug.cgi?id=21802

struct A
{
    auto opAssign(lazy void foo)
    {
        foo();
    }
    auto opOpAssign(string op)(lazy void foo)
    {
        foo();
    }
}

class C
{
    auto opAssign(lazy void foo)
    {
        foo();
    }
    auto opOpAssign(string op)(lazy void foo)
    {
        foo();
    }
}

void bar(int x) { }

void main ()
{
    A a;
    a ~= bar (1); // OK
    a = bar (1); // Error: expression bar(1) is void and has no value

    C c = new C;
    c ~= bar(1);
    c = bar(1);
}
