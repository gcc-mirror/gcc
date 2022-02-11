/*
TEST_OUTPUT:
---
fail_compilation/fail176.d(13): Error: cannot modify `immutable` expression `a[1]`
fail_compilation/fail176.d(16): Error: cannot modify `immutable` expression `b[1]`
fail_compilation/fail176.d(19): Error: cannot modify `const` expression `c[1]`
---
*/

void foo()
{
    auto a = "abc";
    a[1] = 'd';

    immutable char[3] b = "abc";
    b[1] = 'd';

    const char[3] c = "abc";
    c[1] = 'd';
}
