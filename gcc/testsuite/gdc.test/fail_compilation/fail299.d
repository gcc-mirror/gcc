/*
TEST_OUTPUT:
---
fail_compilation/fail299.d(14): Error: initializer provided for struct `Foo` with no fields
---
*/

struct Foo {}

void foo (Foo b, void delegate ()) {}

void main ()
{
    foo(Foo(1), (){});
}
