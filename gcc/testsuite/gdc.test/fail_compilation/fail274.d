/*
TEST_OUTPUT:
---
fail_compilation/fail274.d(10): Error: expression expected not ;
---
*/

void main()
{
    asm { inc [; }
}
