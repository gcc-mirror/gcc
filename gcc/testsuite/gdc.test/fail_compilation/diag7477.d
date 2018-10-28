/*
TEST_OUTPUT:
---
fail_compilation/diag7477.d(13): Error: cannot implicitly convert expression `0` of type `int` to `Foo`
fail_compilation/diag7477.d(20): Error: cannot implicitly convert expression `0` of type `int` to `string`
---
*/

struct Foo { int x; }

enum Bar : Foo
{
    a,
    b,
    c
}

enum Baz : string
{
    a,
    b,
}
