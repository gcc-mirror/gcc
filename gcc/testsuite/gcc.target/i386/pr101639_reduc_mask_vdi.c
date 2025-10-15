/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -O2" } */
/* { dg-final { scan-assembler-times "vptest" 2 } } */
/* { dg-final { scan-assembler-times "sete" 1 } } */
/* { dg-final { scan-assembler-times "setne" 1 } } */
/* { dg-final { scan-assembler-times "popcnt" 1 } } */
/* { dg-final { scan-assembler-times "vmovmskpd" 1 } } */

bool f(long long *p, long n)
{
  bool r = true;
  for(long i = 0; i < 4; ++i)
    r &= (p[i] != 0);
  return r;
}

bool f2(long long *p, long n)
{
  bool r = false;
  for(long i = 0; i < 4; ++i)
    r |= (p[i] != 0);
  return r;
}

bool f3(long long *p, long n)
{
  bool r = false;
  for(long i = 0; i < 4; ++i)
    r ^= (p[i] != 0);
  return r;
}
