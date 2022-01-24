/*
TEST_OUTPUT:
---
fail_compilation/fail299.d(14): Error: more initializers than fields (0) of `Foo`
---
*/

struct Foo {}

void foo (Foo b, void delegate ()) {}

void main ()
{
    foo(Foo(1), (){});
}
