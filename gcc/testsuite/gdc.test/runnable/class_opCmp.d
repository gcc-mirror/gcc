class A
{
    int x;
    this(int a) { x = a; }

    alias opCmp = Object.opCmp;
    alias opCmp = my_cmp;

    final int my_cmp(A a)
    {
        return x - a.x;
    }
}

void main()
{
    auto a1 = new A(1);
    auto a2 = new A(2);
    A a_null = null;
    assert(a1 > a_null);
    assert(a_null < a1);
    assert(!(a1 < a1));
    assert(a1 < a2);
    assert(a2 > a1);
}
