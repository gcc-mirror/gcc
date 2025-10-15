/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-times "kortest" 2 } } */
/* { dg-final { scan-assembler-times "setc" 1 } } */
/* { dg-final { scan-assembler-times "setne" 1 } } */
/* { dg-final { scan-assembler-times "popcnt" 1 } } */

bool f(int * p, long n)
{
  bool r = true;
  for(long i = 0; i < 8; ++i)
    r &= (p[i] != 0);
  return r;
}

bool f2(int * p, long n)
{
  bool r = false;
  for(long i = 0; i < 8; ++i)
    r |= (p[i] != 0);
  return r;
}

bool f3(int * p, long n)
{
  bool r = false;
  for(long i = 0; i < 8; ++i)
    r ^= (p[i] != 0);
  return r;
}
