// REQUIRED_ARGS: -inline -O

// Test operator overloading

extern (C) int printf(const(char*) fmt, ...);

struct Tuple6798(T...)
{
    T field;
    alias field this;

    bool opEquals(Tuple6798 rxx)
    {
        foreach (i, _; T)
        {
            if (!__equals(this[i], rxx[i]))
                assert(0);
                //return false;
        }
        return true;
    }
}

auto tuple(T...)(T args)
{
    return Tuple6798!T(args);
}

int zzzz()
{
    if (!__equals("mno", "mno"))
        assert(0);

    assert(tuple("abcd", "x") == tuple("abcd", "x"));
    return 0;
}

int main()
{
    zzzz();

    printf("Success\n");
    return 0;
}
