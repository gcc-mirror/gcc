// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

int sx;
double sy;

/*
TEST_OUTPUT:
---
fail_compilation/fail13336b.d(16): Error: cast(double)sx is not an lvalue
---
*/
ref f1(bool f)
{
    if (f)
        return sx;
    return sy;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail13336b.d(30): Error: cast(double)sx is not an lvalue
---
*/
ref f2(bool f)
{
    if (f)
        return sy;
    return sx;
}
