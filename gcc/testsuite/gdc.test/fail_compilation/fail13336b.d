// REQUIRED_ARGS: -o-

int sx;
double sy;

/*
TEST_OUTPUT:
---
fail_compilation/fail13336b.d(16): Error: `cast(double)sx` is not an lvalue and cannot be modified
fail_compilation/fail13336b.d(24): Error: `cast(double)sx` is not an lvalue and cannot be modified
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
