// REQUIRED_ARGS: -dip25

template indexOfReturn(T...)
{
    static if (T.length == 0)
    {
        enum indexOfReturn = -1;
    }
    else static if (T[$ - 1] == "return")
    {
        enum indexOfReturn = T.length - 1;
    }
    else
    {
        enum indexOfReturn = indexOfReturn!(T[0..$-1]);
    }
}

struct Test
{
    int n;

    ref int getN() return
    {
        return n;
    }

    int getNNonReturn()
    {
        return n;
    }
}

void main()
{
    assert(indexOfReturn!(__traits(getFunctionAttributes, Test.getN)) != -1);
    assert(indexOfReturn!(__traits(getFunctionAttributes, Test.getNNonReturn)) == -1);
}
