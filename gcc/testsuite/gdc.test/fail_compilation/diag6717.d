/*
TEST_OUTPUT:
---
fail_compilation/diag6717.d(12): Error: end of instruction expected, not 'h'
---
*/

void main()
{
    asm
    {
        mov AX, 12h ;
    }
}
