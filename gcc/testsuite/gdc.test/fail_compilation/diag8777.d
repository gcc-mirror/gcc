/*
TEST_OUTPUT:
---
fail_compilation/diag8777.d(12): Error: constructor `diag8777.Foo1.this` missing initializer for immutable field `x`
fail_compilation/diag8777.d(12): Error: constructor `diag8777.Foo1.this` missing initializer for const field `y`
---
*/
class Foo1
{
    immutable int[5] x;
    const int[5] y;
    this() {}
}

/*
TEST_OUTPUT:
---
fail_compilation/diag8777.d(25): Error: cannot modify `immutable` expression `x`
fail_compilation/diag8777.d(28): Error: cannot modify `const` expression `y`
---
*/
void test2()
{
    immutable int x;
    x = 1;

    const int y;
    y = 1;
}

/*
TEST_OUTPUT:
---
fail_compilation/diag8777.d(42): Error: cannot remove key from `immutable` associative array `hashx`
fail_compilation/diag8777.d(43): Error: cannot remove key from `const` associative array `hashy`
---
*/
immutable(int[int]) hashx;
const(int[int]) hashy;
void test3()
{
    hashx.remove(1);
    hashy.remove(1);
}
