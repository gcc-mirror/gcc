

template AliasSeq(T...) { alias AliasSeq = T; }

template staticMap(alias F, T...)
{
    alias A = AliasSeq!();
    static foreach (t; T)
        A = AliasSeq!(A, F!t);
    alias staticMap = A;
}

template Qual(alias T)
{
    alias Qual = T;
}

void test()
{
    int x = 3;
    int y = 4;

    alias XY = staticMap!(Qual, x, y);
    assert(XY[0] == 3);
    assert(XY[1] == 4);
}

void main()
{
    test();
}
