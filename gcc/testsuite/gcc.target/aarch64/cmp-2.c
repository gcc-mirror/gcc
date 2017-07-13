/* { dg-do compile } */
/* { dg-options "-O2" } */

int lt (int x, int y)
{
  if ((x - y) < 0)
    return 10;

  return 0;
}

int ge (int x, int y)
{
  if ((x - y) >= 0)
    return 10;

  return 0;
}

/* { dg-final { scan-assembler-times "csel\t" 2 } } */
/* { dg-final { scan-assembler-not "sub\t" } } */
