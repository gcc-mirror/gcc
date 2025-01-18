/*
TEST_OUTPUT:
---
fail_compilation/fail21928.d(18): Error: this array literal causes a GC allocation in `@nogc` function `main`
---
*/

@nogc:


struct Shape
{
    immutable size_t[] dims = [];
}

void main()
{
    auto s = Shape(2 ~ Shape.init.dims);
}
