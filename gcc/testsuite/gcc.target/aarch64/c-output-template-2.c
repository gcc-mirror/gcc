/* { dg-do compile } */

void
test (void)
{
    __asm__ ("@ %c0" : : "S" (test));
}

/* { dg-final { scan-assembler "@ test" } } */
