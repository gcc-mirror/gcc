/*
TEST_OUTPUT:
---
fail_compilation/fail9891.d(13): Error: cast(int)i is not an lvalue
fail_compilation/fail9891.d(18): Error: cast(int)i is not an lvalue
fail_compilation/fail9891.d(23): Error: prop() is not an lvalue
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
