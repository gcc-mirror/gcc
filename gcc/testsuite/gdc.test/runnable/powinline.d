/*
REQUIRED_ARGS: -betterC
RUN_OUTPUT:
---
Success
---
*/
import core.stdc.stdio;

void test1()
{
    enum real Two = 2.0;
    static assert(Two^^3 == 8.0);
}

void test2()
{
    double x = 5.0;
    assert(x^^-1 == 1/x);
    x = -1.0;
    assert(x^^1 == x);
    assert((x += 3) ^^ 2.0 == 4.0);
    assert((x) ^^ 2.0 == 4.0);
    assert((x *= 5) ^^ 2.0 == (x * x));
    assert(x^^-1 == 1.0 / x);
    assert((x^^-1) ^^ 0.0 == 1.0);
}

void test3()
{
    int x = 6;
    assert(x ^^ 0 == 1);
    assert((x += 3) ^^ 2 == 81);
    assert(x ^^ 2 == (x ^^ 1) * (x ^^ 1));
    static assert(4.0 ^^ -1 == 0.25);
}

void test4()
{
    // Test that LHS side effects are evaluated.
    int count = 0;
    double x()
    {
        count++;
        return 8.0;
    }
    assert(x ^^ -1 == 1.0 / 8.0);
    assert(count == 1);
    assert(x ^^ 0 == 1.0);
    assert(count == 2);
    assert(x ^^ 1 == 8.0);
    assert(count == 3);
    assert(x ^^ 2 == 64.0);
    assert(count == 4);
}

extern(C) void main()
{
    test1();
    test2();
    test3();
    test4();
    printf("Success\n");
}
