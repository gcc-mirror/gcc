// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/fail13938.d(14): Error: cannot directly load TLS variable 'val'
---
*/

void test1()
{
    static int val;
    asm
    {
        mov EAX, val;
    }
}
