/*
TEST_OUTPUT:
---
fail_compilation/diag7477.d(13): Error: integral constant must be scalar type, not `Foo`
fail_compilation/diag7477.d(20): Error: integral constant must be scalar type, not `string`
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
