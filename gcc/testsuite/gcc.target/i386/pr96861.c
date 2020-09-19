/* { dg-do compile } */
/* { dg-options "-O2 -mstv -march=skylake-avx512 -mtune=skylake-avx512" } */

#define max(a,b) (((a) > (b))? (a) : (b))
#define min(a,b) (((a) < (b))? (a) : (b))

int smax1(int x)
{
  return max(x,1);
}

int smin1(int x)
{
  return min(x,1);
}

int smaxm1(int x)
{
  return max(x,-1);
}

int sminm1(int x)
{
  return min(x,-1);
}

unsigned int umax1(unsigned int x)
{
  return max(x,1);
}

unsigned int umin1(unsigned int x)
{
  return min(x,1);
}

/* { dg-final { scan-assembler-times "test" 6 } } */
/* { dg-final { scan-assembler-not "cmp" } } */
