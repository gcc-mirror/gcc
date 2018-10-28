/*
TEST_OUTPUT:
---
fail_compilation/diag7050b.d(12): Error: pure function 'diag7050b.f.g' cannot call impure function 'diag7050b.f'
---
*/

void f()
{
    pure void g()
    {
        f();
    }
}
