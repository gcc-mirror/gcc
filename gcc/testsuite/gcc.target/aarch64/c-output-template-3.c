/* { dg-do compile } */
/* { dg-options "-Wno-pointer-arith" } */

void
test (void)
{
    __asm__ ("@ %c0" : : "S" (&test + 4));
}

/* { dg-final { scan-assembler "@ test\\+4" } } */
