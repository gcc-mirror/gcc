// PERMUTE_ARGS:
// REQUIRED_ARGS: -version=3 -version=foo

extern(C) int printf(const char*, ...);

/*******************************************/

void test1()
{
    int i = 3;

    version(2)
    {
        i = 2;
    }
    else
    {
        i = 0;
    }
    printf("i = %d\n", i);
    assert(i == 2);

    i = 3;

    version(foo)
    {
        i = 2;
    }
    else
    {
        i = 0;
    }
    printf("i = %d\n", i);
    assert(i == 2);
}

/*******************************************/

version(foo)
{
    version = bar;
}
else
{
    version = 4;
}

void test2()
{
    version(bar)
    {
    }
    else
        assert(0);

    version(4) assert(0);
}

/*******************************************/

int main()
{
    test1();
    test2();
    return 0;
}

