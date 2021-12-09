/*
TEST_OUTPUT:
---
fail_compilation/fail9891.d(13): Error: expression `i` of type `immutable(int)` is not implicitly convertible to type `ref int` of parameter `n`
fail_compilation/fail9891.d(18): Error: expression `i` of type `immutable(int)` is not implicitly convertible to type `out int` of parameter `n`
fail_compilation/fail9891.d(23): Error: `prop()` is not an lvalue and cannot be modified
---
*/

immutable int i;
int prop() { return 0; }

void f1(ref int n = i)
{
    ++n;
}

void f2(out int n = i)
{
    ++n;
}

void f3(ref int n = prop)
{
    ++n;
}
