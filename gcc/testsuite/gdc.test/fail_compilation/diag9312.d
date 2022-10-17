/*
TEST_OUTPUT:
---
fail_compilation/diag9312.d(10): Error: `with` expressions must be aggregate types or pointers to them, not `int`
---
*/

void main()
{
    with (1)
    {
    }
}
