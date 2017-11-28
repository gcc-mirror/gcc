/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mcet" } */
/* { dg-final { scan-assembler-times {\mendbr} 2 } } */

extern int x;

void
test (int i)
{
  x = i;
}

void
bar (int i)
{
  test (i);
}
