/*
TEST_OUTPUT:
---
fail_compilation/ice11793.d(11): Error: circular reference to `ice11793.Outer.outer`
---
*/

class Outer
{
    int foo;
    Outer outer = new Outer();
}
