/* { dg-do compile } */

void
test (void)
{
    __asm__ ("@ %c0" : : "i" (42));
}

/* { dg-final { scan-assembler "@ 42" } } */
