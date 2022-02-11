/*
TEST_OUTPUT:
---
fail_compilation/fail86.d(12): Error: alias `Foo` recursive alias declaration
---
*/

template Foo(TYPE) {}

void main()
{
    alias Foo!(int) Foo;
}
