/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mcet" } */
/* { dg-final { scan-assembler-times {\mendbr} 2 } } */

extern int x;

 __attribute__ ((visibility ("hidden")))
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
