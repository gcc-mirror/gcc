/*
TEST_OUTPUT:
---
fail_compilation/diag13320.d(13): Error: 'f += 1' is not a scalar, it is a Foo
---
*/

struct Foo {}

void main()
{
    Foo f;
    ++f;
}
