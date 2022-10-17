module imports.gdc170;

class bar(T)
{
    void undefined_reference() {}
}

template foo(T)
{
    bar!T foo1(T2)() if (true) do { return null; }
    bar!T foo2(T2)() { return null; }
    bar!T foo3(T2 = void)() if (true) do { return null; }
    bar!T foo4(T2 = void)() { return null; }
    void foo5(T2)(bar!T x) if (true) do {}
    void foo6(T2)(bar!T x) {}
    void foo7(T2 = void)(bar!T x) if (true) do {}
    void foo8(T2 = void)(bar!T x) {}
}
