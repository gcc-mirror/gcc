/*
TEST_OUTPUT:
---
block displacement of -130 exceeds the maximum offset of -128 to 127.
---
*/

void foo()
{
    enum NOP = 0x9090_9090_9090_9090;

    version(GNU)
    {
        version(X86) asm {
            "L1:"
            "dq %0,%0,%0,%0;"
            "dq %0,%0,%0,%0;"
            "dq %0,%0,%0,%0;"
            "dq %0,%0,%0,%0;"
            "loop L1;" : "n" (NOP) : : ;
        }
        else version(X86_64) asm {
            "L1:"
            "dq %0,%0,%0,%0;"
            "dq %0,%0,%0,%0;"
            "dq %0,%0,%0,%0;"
            "dq %0,%0,%0,%0;"
            "loop L1;" : "n" (NOP) : : ;
        }
        else static assert(false, "ASM code not implemented for this architecture");
    }
    else asm
    {
    L1:
        dq NOP,NOP,NOP,NOP;    //  32
        dq NOP,NOP,NOP,NOP;    //  64
        dq NOP,NOP,NOP,NOP;    //  96
        dq NOP,NOP,NOP,NOP;    // 128
        // unnoticed signed underflow of rel8 with DMD2.056
        loop L1;
    }
}
