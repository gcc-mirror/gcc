// PERMUTE_ARGS: -fPIC -O

extern (C) int printf(const char*, ...);

/***************************************************/

align(16) struct S41
{
    int[4] a;
}

shared int x41;
shared S41 s41;

void test11310()
{
    printf("&x = %p\n", &x41);
    printf("&s = %p\n", &s41);
    assert((cast(int)&s41 & 0xF) == 0);
}

/***************************************************/


struct S17034
{
@nogc pure nothrow:
    private long v;
    void foo()
    {
        v >>>= 1;
        if (!v)
            return;
        v >>>= 1;
    }
}

void test17034()
{
    auto s = S17034(1L);
    s.foo();
    assert(s.v == 0);
}

/***************************************************/

int main()
{
    test11310();
    test17034();

    printf("Success\n");
    return 0;
}
