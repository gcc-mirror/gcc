/* { dg-do compile } */
/* { dg-additional-options "-O3 -mno-mmx -march=icelake-server" } */
/* { dg-final { scan-assembler-times "vpbroadcastb" 1 } } */

signed char a;
signed char f (int i, int j)
{
  signed char c;
  while (i != 0)
  {
    a ^= j;
    ++c;
    ++i;
  }
  return c;
}
