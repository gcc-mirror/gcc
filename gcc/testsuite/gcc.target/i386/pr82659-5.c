/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mcet" } */
/* { dg-final { scan-assembler-times {\mendbr} 1 } } */

static void
test (void)
{
}

void (*test_p) (void) = test;
