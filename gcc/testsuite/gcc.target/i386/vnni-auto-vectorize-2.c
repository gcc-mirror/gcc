/* { dg-do run } */
/* { dg-options "-O2 -mavx512vnni -mavx512vl" } */
/* { dg-require-effective-target avx512vnni } */
/* { dg-require-effective-target avx512vl } */

static void vnni_test (void);
#define DO_TEST vnni_test
#define AVX512VNNI
#define AVX512VL
#include "avx512f-check.h"
#include "vnni-auto-vectorize-1.c"

#define N 256
unsigned char a_u8[N];
char b_i8[N];
short a_i16[N], b_i16[N];
int i8_exp, i8_ref, i16_exp, i16_ref;

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
sdot_prod_hi_scalar (short * restrict a, short * restrict b,
		     int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

int __attribute__((noinline, noclone, optimize("no-tree-vectorize")))
usdot_prod_qi_scalar (unsigned char * restrict a, char *restrict b,
	       int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

void init()
{
  int i;

  i8_exp = i8_ref = 127;
  i16_exp = i16_ref = 65535;

  for (i = 0; i < N; i++)
    {
      a_u8[i] = (i + 3) % 256;
      b_i8[i] = (i + 1) % 128; 
      a_i16[i] = i * 2;
      b_i16[i] = -i + 2;
    }
}

static void vnni_test()
{
  init ();
  i16_exp = sdot_prod_hi (a_i16, b_i16, i16_exp, N);
  i16_ref = sdot_prod_hi_scalar (a_i16, b_i16, i16_ref, N);
  if (i16_exp != i16_ref)
    abort ();

  init ();
  i8_exp = usdot_prod_qi (a_u8, b_i8, i8_exp, N);
  i8_ref = usdot_prod_qi_scalar (a_u8, b_i8, i8_ref, N);
  if (i8_exp != i8_ref)
    abort ();
}
