// REQUIRED_ARGS:

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=19479

mixin template genInts19479a()
{
    static foreach (t; 0..1)
        int i = 5;
}

mixin template genInts19479b()
{
    static foreach (t; 0..2)
        mixin("int i" ~ cast(char)('0' + t) ~ " = 5;");
}

void test19479()
{
    {
        static foreach (t; 0..1)
            int i = 5;
        assert(i == 5);
    }
    {
        mixin genInts19479a!();
        assert(i == 5);
    }
    {
        static foreach (t; 0..2)
            mixin("int i" ~ cast(char)('0' + t) ~ " = 5;");
        assert(i0 == 5);
        assert(i1 == 5);
    }
    {
        mixin genInts19479b!();
        assert(i0 == 5);
        assert(i1 == 5);
    }
}

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=23192

alias AliasSeq(Args...) = Args;

struct S23192
{
    int x;
    int y;

    int fun()
    {
        static foreach (sym; AliasSeq!(S23192.x))
            int i = sym;

        static foreach (sym; AliasSeq!(this.y))
            int j = sym;

        return i + j;
    }
}

void test23192()
{
    assert(S23192(1, 2).fun() == 3);
    static assert(S23192(1, 2).fun() == 3);
}

void main()
{
    test19479();
    test23192();
}
