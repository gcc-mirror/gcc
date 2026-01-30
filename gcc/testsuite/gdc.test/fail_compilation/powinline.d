/*
TEST_OUTPUT:
---
fail_compilation/powinline.d(25): Error: cannot implicitly convert expression `(a + 5.0) ^^ 2L` of type `double` to `int`
fail_compilation/powinline.d(26): Error: cannot implicitly convert expression `(1.0 / foo()) ^^ 2L` of type `double` to `int`
fail_compilation/powinline.d(31): Error: void has no value
fail_compilation/powinline.d(31): Error: incompatible types for `(5.0) * (bar())`: `double` and `void`
fail_compilation/powinline.d(37): Error: cannot modify `immutable` expression `a`
---
*/

double foo()
{
    return 5.0;
}

void bar()
{
    return;
}

void test1()
{
    double a = 2.0;
    int b = (a + 5.0) ^^ 2.0;
    b = (1 / foo()) ^^ 2.0;
}

void test2()
{
    double a = (5.0 * bar()) ^^ 2.0;
}

void test3()
{
    immutable double a = 3.0;
    (a ^^= 2.0) = 6;
}
