/* { dg-do compile } */
/* { dg-options "-O2 " } */

int f3 (int x, int y)
{
  int res = x << 3;
  return res != 0;
}

/* We should combine the shift and compare */
/* { dg-final { scan-assembler "cmp\.*\twzr, w\[0-9\]+, lsl 3" } } */
