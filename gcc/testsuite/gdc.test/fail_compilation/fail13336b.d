// REQUIRED_ARGS: -o-

int sx;
double sy;

/*
TEST_OUTPUT:
---
fail_compilation/fail13336b.d(16): Error: cannot `ref` return expression `cast(double)sx` because it is not an lvalue
fail_compilation/fail13336b.d(24): Error: cannot `ref` return expression `cast(double)sx` because it is not an lvalue
---
*/
ref f1(bool f)
{
    if (f)
        return sx;
    return sy;
}

ref f2(bool f)
{
    if (f)
        return sy;
    return sx;
}
