/*
TEST_OUTPUT:
---
fail_compilation/diag8894.d(16): Error: no property `x` for `f` of type `diag8894.Foo`
fail_compilation/diag8894.d(17): Error: no property `y` for `f` of type `diag8894.Foo`
fail_compilation/diag8894.d(18): Error: no property `x` for `f` of type `diag8894.Foo`
fail_compilation/diag8894.d(19): Error: no property `x` for `f` of type `diag8894.Foo`
---
*/

struct Foo { }

void main()
{
    Foo f;
    f.x;           // UFCS getter1
    f.y!int;       // UFCS getter2
    f.x     = 10;  // UFCS setter1
    f.x!int = 10;  // UFCS setter2
}
