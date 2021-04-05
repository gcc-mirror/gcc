
alias Int = mixin("int");
alias Lint = mixin("Int");

int test1(mixin("int")* p)
{
    mixin("int")[] a;
    mixin("int[]") b;
    mixin("int[] c;");
    mixin("*p = c[0];");
    *p = mixin("c[0]");
    return *p + a[0] + b[0] + c[0];
}

/******************************************/

void test2()
{
    auto a = __traits(allMembers, mixin(__MODULE__));
}

/*****************************************/

void test3()
{
    char val;
    int mod;
    enum b = __traits(compiles, mixin("*cast(int*)&val + mod"));
    static assert(b == true);
}

/********************************************/


struct S
{
    int fielda;
    int fieldb;
}

template Foo4(alias T)
{
    enum Foo4 = true;
}

void test4()
{
    S sa;
    auto a = Foo4!( __traits(getMember,sa,"fielda") );

    S sb;
    enum getStuff = q{ __traits(getMember,sb,"fieldb") };
    auto b = Foo4!(mixin(getStuff));
}

