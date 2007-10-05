/* { dg-mips-options "-mabi=64 -O2" } */

NOMIPS16 long double __powitf2 (long double x, int m)
{
  long double y = x;
  while (m >>= 1)
    {
      x = x * x;
      if (m % 2)
        y = y * x;
    }
  return y;
}
