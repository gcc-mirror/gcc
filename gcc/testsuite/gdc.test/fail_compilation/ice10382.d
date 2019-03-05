/*
TEST_OUTPUT:
---
fail_compilation/ice10382.d(14): Error: can only catch class objects, not `int`
---
*/

void main ()
{
    try
    {
        int b = 3;
    }
    catch (int a) { }
}
