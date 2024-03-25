// { dg-do compile }
// { dg-options "-Wunused-value" }

@safe pure nothrow T t1(T)(T x)
{
    return x * x;
}

nothrow pure int f1(immutable(int)[] a)
{
    return 0;
}

nothrow pure int f2(immutable(int)*  p)
{
    return 0;
}

void test()
{
    int x = 3;
    t1(x); // { dg-warning "without side effects discards return value" }

    auto fp = &t1!int;
    fp(x); // { dg-warning "without side effects discards return value" }

    f1([]); // { dg-warning "without side effects discards return value" }
    f2(null); // { dg-warning "without side effects discards return value" }
}
