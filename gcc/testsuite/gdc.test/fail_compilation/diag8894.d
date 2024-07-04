/*
TEST_OUTPUT:
---
fail_compilation/diag8894.d(20): Error: no property `x` for `f` of type `diag8894.Foo`
fail_compilation/diag8894.d(15):        struct `Foo` defined here
fail_compilation/diag8894.d(21): Error: no property `y` for `f` of type `diag8894.Foo`
fail_compilation/diag8894.d(15):        struct `Foo` defined here
fail_compilation/diag8894.d(22): Error: no property `x` for `f` of type `diag8894.Foo`
fail_compilation/diag8894.d(15):        struct `Foo` defined here
fail_compilation/diag8894.d(23): Error: no property `x` for `f` of type `diag8894.Foo`
fail_compilation/diag8894.d(15):        struct `Foo` defined here
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
