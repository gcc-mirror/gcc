/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection=check" } */
/* { dg-final { scan-assembler-not "endbr" } } */

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
