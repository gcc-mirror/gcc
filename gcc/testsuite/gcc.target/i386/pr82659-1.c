/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-final { scan-assembler-times {\mendbr} 1 } } */

extern int x;

static void
__attribute__ ((noinline, noclone))
test (int i)
{
  x = i;
}

void
bar (int i)
{
  test (i);
}
