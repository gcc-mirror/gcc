/*
TEST_OUTPUT:
---
fail_compilation/fail190.d(9): Error: cannot have pointer to `(int, int, int)`
fail_compilation/fail190.d(16): Error: template instance `fail190.f!(int, int, int)` error instantiating
---
*/

T* f(T...)(T x)
{
    return null;
}

void main()
{
    auto x = f(2,3,4);
    *x = *x;
}
