/*
TEST_OUTPUT:
---
fail_compilation/ctypes.d(11): Error: use `real` instead of `long double`
fail_compilation/ctypes.d(12): Error: use `long` for a 64 bit integer instead of `long long`
---
*/

void test()
{
    long double r;
    long long ll;
}
