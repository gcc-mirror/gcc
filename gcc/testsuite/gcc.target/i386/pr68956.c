/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -mfpmath=sse -mavx2 -ftree-vectorize" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

extern void abort (void);

int l;

static void __attribute__((noclone,noinline))
test1 (double *in1, double *in2, double *out,
       int l1, int l2, int *n)
{
  double sum;
  int na = n[0];
  int nb = n[1];
  int i;
  _Bool ic, jc;

  jc = (l > na) && (l > nb);
  for (int i = 0; i < l2; i++)
    {
      ic = (i <= na) && (i <= nb);
      sum = 0;
      if (ic && jc)
	sum = in1[i] + in2[i];
      out[i] = sum;
    }
}

static void
avx2_test (void)
{
  double in1[40], in2[40], out[40], sum;
  int n[2],l1,l2,i,na,nb;
  _Bool ic, jc;

  l = 0;
  l1 = 8;
  l2 = 40;
  n[0] = 14;
  n[1] = 13;

  for (i = 0; i < l2; i++)
    {
      in1[i] = i;
      in2[i] = i;
      out[i] = 0;
    }

  test1 (in1, in2, out, l1, l2, n);

  na = n[0];
  nb = n[1];

  jc = (l > na) && (l > nb);
  for (int i = 0; i < l2; i++)
    {
      ic = (i <= na) && (i <= nb);
      sum = 0;
      if (ic && jc)
	sum = in1[i] + in2[i];
      if (out[i] != sum)
	abort ();
    }
}
