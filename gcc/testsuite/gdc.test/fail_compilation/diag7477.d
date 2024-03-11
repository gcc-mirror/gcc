/*
TEST_OUTPUT:
---
fail_compilation/diag7477.d(13): Error: cannot generate 0 value of type `Foo` for `a`
fail_compilation/diag7477.d(20): Error: cannot generate 0 value of type `string` for `a`
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
