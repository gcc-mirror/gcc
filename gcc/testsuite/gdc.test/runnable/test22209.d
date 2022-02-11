// https://issues.dlang.org/show_bug.cgi?id=22209

struct U { size_t[4] a; }

struct S
{
    int x;
    U u;
    alias u this;
}

U foo()
{
    S s = S(42, U([1, 2, 3, 4]));
    return s;
}

void main()
{
    assert(foo().a[0] == 1);
}
