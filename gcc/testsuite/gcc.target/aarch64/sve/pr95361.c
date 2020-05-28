/* { dg-options "-O2" } */

__SVInt8_t
f (__SVInt8_t x, int y)
{
  if (y == 1)
    asm volatile ("" ::: "z8");
  if (y == 2)
    asm volatile ("" ::: "z9");
  return x;
}
