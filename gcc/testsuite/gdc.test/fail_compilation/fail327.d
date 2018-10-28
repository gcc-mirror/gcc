/*
TEST_OUTPUT:
---
fail_compilation/fail327.d(10): Error: asm statement is assumed to be @system - mark it with '@trusted' if it is not
---
*/

@safe void foo()
{
    version(GNU)
    {
        version(X86) asm {"xor %%EAX,%%EAX" : : : ;}
        else version(X86_64) asm {"xor %%EAX,%%EAX" : : : ;}
        else static assert(false, "ASM code not implemented for this architecture");
    }
    else asm { xor EAX,EAX; }
}
