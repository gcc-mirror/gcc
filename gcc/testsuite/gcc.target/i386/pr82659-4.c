/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mcet" } */
/* { dg-final { scan-assembler-times {\mendbr} 2 } } */

static void
test (void)
{
}

void *
bar (void)
{
  return test;
}
