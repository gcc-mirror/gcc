/*
TEST_OUTPUT:
---
fail_compilation/diag9312.d(10): Error: `with` expression types must be enums or aggregates or pointers to them, not `int`
---
*/

void main()
{
    with (1)
    {
    }
}
