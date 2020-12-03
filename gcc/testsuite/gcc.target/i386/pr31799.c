/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (int x, int *y, int *z)
{
  *z = ++x;
  if (x != 0)
  *y = 1;
}

/* { dg-final { scan-assembler-not "test" } } */
