/*
TEST_OUTPUT:
---
fail_compilation/diag10405.d(10): Error: cannot return non-void from void function
---
*/

void main()
{
    return 10;
}
