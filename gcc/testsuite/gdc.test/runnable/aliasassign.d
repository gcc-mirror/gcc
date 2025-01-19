

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

void test1()
{
    int x = 3;
    int y = 4;

    alias XY = staticMap!(Qual, x, y);
    assert(XY[0] == 3);
    assert(XY[1] == 4);
}

/**********************************************/

struct T
{
    int k,i = 2;
}

struct S
{
    int x;
    T t;
    alias ti = t.i;
}

void test2()
{
    T t = T(1, 2);
    S s;
    assert(s.ti == 2);
}

/**********************************************/

int main()
{
    test1();
    test2();

    return 0;
}
