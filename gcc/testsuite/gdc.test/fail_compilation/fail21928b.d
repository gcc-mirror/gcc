/*
TEST_OUTPUT:
---
fail_compilation/fail21928b.d(18): Error: this array literal causes a GC allocation in `@nogc` function `main`
---
*/

@nogc:


struct Shape
{
    immutable size_t[] dims = [];
}

void main()
{
    auto s = Shape(Shape.init.dims ~ 2);
}
