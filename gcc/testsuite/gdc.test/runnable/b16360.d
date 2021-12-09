// REQUIRED_ARGS: -inline

pragma(inline, true)
auto foo()
{
    static struct U
    {
        int a = 42;
        float b;
    }
    U u;
    return u.a;
}

pragma(inline, true)
T bitCast(T, S)(auto ref S s)
{
    union BitCaster
    {
        S ss;
        T tt;
    }
    BitCaster bt;
    bt.ss = s;
    return bt.tt;
}

pragma(inline, true)
int withFuncCalls()
{
    static struct WithFuncCalls
    {
        int v;
        pragma(inline, true)
        int call(){return v;}
        pragma(inline, true)
        void otherCall(){v++;}
    }
    auto bt = WithFuncCalls(50);
    bt.v += -9;
    bt.otherCall();
    return bt.call();
}

void main()
{
    assert(foo == 42);
    assert(bitCast!int(1.0f) == 0x3f800000);
    assert(withFuncCalls() == 42);
}
