// Issue 2920 - recursive templates blow compiler stack
// template_29_B.

template foo(uint i)
{
    static if (i > 0)
    {
        const uint bar = foo!(i - 1).bar;
    }
    else
    {
        const uint bar = 1;
    }
}
int main()
{
    return foo!(uint.max).bar;
}
