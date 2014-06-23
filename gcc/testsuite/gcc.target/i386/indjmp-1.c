/* { dg-do compile  { target ia32 } } */
/* { dg-options "-O2" } */

#define ADVANCE_AND_DISPATCH() goto *addresses[*pc++]

void
Interpret(const unsigned char *pc)
{
    static const void *const addresses[] = {
      &&l0, &&l1, &&l2
    };

l0:
    ADVANCE_AND_DISPATCH();

l1:
    ADVANCE_AND_DISPATCH();

l2:
    return;
}

/* { dg-final { scan-assembler-not "jmp\[ \t\]*.%eax" } } */
