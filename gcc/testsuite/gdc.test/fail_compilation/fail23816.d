// https://issues.dlang.org/show_bug.cgi?id=23816

/*
TEST_OUTPUT:
---
fail_compilation/fail23816.d(14): Error: opcode expected, not `NOP`
---
*/

void main()
{
    asm
    {
        NOP;
    }
}
