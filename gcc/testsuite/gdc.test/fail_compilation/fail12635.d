/*
TEST_OUTPUT:
---
fail_compilation/fail12635.d(19): Error: Cannot generate a segment prefix for a branching instruction
---
*/

void foo()
{
    enum NOP = 0x9090_9090_9090_9090;

    asm
    {
    L1:
        dq NOP,NOP,NOP,NOP;    //  32
        dq NOP,NOP,NOP,NOP;    //  64
        dq NOP,NOP,NOP,NOP;    //  96
        dq NOP,NOP,NOP,NOP;    // 128
        jmp DS:L1;
    }
}
