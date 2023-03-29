/* { dg-do run } */
/* { dg-options "-O2 -mavxvnniint8" } */
/* { dg-require-effective-target avxvnniint8 } */

#define AVXVNNIINT8
#ifndef CHECK
#define CHECK "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include CHECK
#include "vnniint8-auto-vectorize-1.c"

#define N 256
char a_i8[N], b_i8[N];
unsigned char c_u8[N], d_u8[N];
int i8_exp, i8_ref;

int __attribute__((noipa, optimize("no-tree-vectorize")))
sdot_prod_qi_scalar (char * restrict a, char * restrict b,
		     int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

int __attribute__((noipa, optimize("no-tree-vectorize")))
udot_prod_qi_scalar (unsigned char * restrict a, unsigned char *restrict b,
		     int c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      c += ((int) a[i] * (int) b[i]);
    }
  return c;
}

void init ()
{
  int i;

  i8_exp = i8_ref = 127;

  for (i = 0; i < N; i++)
    {
      a_i8[i] = (-i + 4) % 128;
      b_i8[i] = (i + 1) % 128;
      c_u8[i] = (i + 3) % 256;
      d_u8[i] = (i + 5) % 256;
    }
}

void
TEST (void)
{
  init ();
  i8_exp = sdot_prod_qi (a_i8, b_i8, i8_exp, N);
  i8_ref = sdot_prod_qi_scalar (a_i8, b_i8, i8_ref, N);
  if (i8_exp != i8_ref)
    abort ();

  init ();
  i8_exp = udot_prod_qi (c_u8, d_u8, i8_exp, N);
  i8_ref = udot_prod_qi_scalar (c_u8, d_u8, i8_ref, N);
  if (i8_exp != i8_ref)
    abort ();
}
