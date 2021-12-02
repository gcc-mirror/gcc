struct NullableRef15292(T)
{
    inout(T) get() inout
    {
        assert(false);
    }

    alias get this;
}

struct S15292
{
    NullableRef15292!S15292 n;
}

void main()
{
    S15292 s;
    assert(s == s);
}
