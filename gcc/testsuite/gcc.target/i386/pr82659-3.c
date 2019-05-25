/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-final { scan-assembler-times {\mendbr} 2 } } */

extern int x;

static void
__attribute__ ((noinline, noclone))
test (int i)
{
  x = i;
}

extern __typeof (test) foo __attribute__ ((alias ("test")));

void
bar (int i)
{
  test (i);
}
