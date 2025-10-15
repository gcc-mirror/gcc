/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -O2" } */
/* { dg-final { scan-assembler-times "vptest" 1 } } */
/* { dg-final { scan-assembler-times "sete" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpeq" 1 } } */

bool f2(char * p, long n)
{
  bool r = true;
  for(long i = 0; i < 32; ++i)
    r &= (p[i] != 0);
  return r;
}

