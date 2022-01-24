// https://issues.dlang.org/show_bug.cgi?id=19774
C bar()
{
    return C(42);
}

C foo()
{
    return bar()[1];
}

C gun()
{
    return bar()[$];
}

struct C
{
    int x;

    ~this()
    {
        x = 0;
    }

    int opDollar()
    {
        return 1;
    }

    C opIndex(int a)
    {
        return this;
    }
}

void main()
{
    auto c = foo();
    assert(c.x == 42); /* fails; should pass */
    auto d = gun();
    assert(d.x == 42);
}
