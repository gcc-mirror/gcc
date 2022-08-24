/*
PERMUTE_ARGS:
REQUIRED_ARGS: -version=foo
RUN_OUTPUT:
---
i = 2
---
*/

extern(C) int printf(const char*, ...);

/*******************************************/

void test1()
{
    int i = 3;
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

void test2()
{
    version(bar)
    {
    }
    else
        assert(0);
}

/*******************************************/

int main()
{
    test1();
    test2();
    return 0;
}
