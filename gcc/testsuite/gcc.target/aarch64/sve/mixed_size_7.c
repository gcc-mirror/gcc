/* Originally gcc.dg/vect/bb-slp-6.c */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256 -fno-vect-cost-model" } */

#define N 16

unsigned int out[N];
unsigned int in[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

__attribute__ ((noinline)) int
main1 (unsigned int x, unsigned int y)
{
  int i;
  unsigned int *pin = &in[0];
  unsigned int *pout = &out[0];
  unsigned int a0, a1, a2, a3;

  a0 = *pin++ + 23;
  a1 = *pin++ + 142;
  a2 = *pin++ + 2;
  a3 = *pin++ + 31;

  *pout++ = a0 * x;
  *pout++ = a1 * y;
  *pout++ = a2 * x;
  *pout++ = a3 * y;

  return 0;
}
