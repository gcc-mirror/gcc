// PERMUTE_ARGS:
// https://issues.dlang.org/show_bug.cgi?id=20687

struct S
{
    void foo(){}
}

void main()
{
    S i;
    enum fpe = &S.foo;
    const fp = &S.foo;
    const dg = &i.foo;

    // Allow these
    static dgfp = &S.foo;
    static const dgfpc = &S.foo;
    static immutable dgfpi = &S.foo;
    __gshared dgfpg = &S.foo;
    __gshared const dgfpgc = &S.foo;
    __gshared immutable dgfpgi = &S.foo;

    static foreach (v; [fp, dgfpc, dgfpi, dgfpgc, dgfpgi])
        static assert(fpe == v);

    assert(fp == dg.funcptr && fp == dgfp && fp == dgfpg);
}
