module imports.ice10086x;

import imports.ice10086y;

struct S1
{
    int a1 = 123;
}

@safe auto f1(S1 r)
{
    return r;
}

@safe auto f2a()(S1 r)
{
    return bind!(f1, r);
}

@safe auto f2b(S1 r)
{
    return bind!(f1, r);
}

void test()
{
    S1 s1;

    auto za = bind!(f2a, s1)();
    assert(za.a1 == 123);

    auto zb = bind!(f2b, s1)();
    assert(zb.a1 == 123);
}
