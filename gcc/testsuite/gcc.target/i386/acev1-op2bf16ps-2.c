/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target acev1 } */
/* { dg-options "-O2 -macev1" } */
#define DO_TEST test_acev1_op2bf16ps
void test_acev1_op2bf16ps ();
#include "ace-helper.h"

void calc_op2bf16ps (__tile *dst, __bf16 *src1, __bf16 *src2)
{
  int i, j;

  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j++)
      {
	dst->a[i * 16 + j] += (float) src1[2 * i] * (float) src2[2 * j];
	dst->a[i * 16 + j] += (float) src1[2 * i + 1] * (float) src2[2 * j + 1];
      }
}

void test_acev1_op2bf16ps ()
{
  __tilecfg cfg;
  __tile dst_ref;
  __bsr bsr0;
  union512bf16_bf src1, src2;
  int m;

  for (m = 0; m < 32; m++)
    {
      int sign;
      sign = m % 2 ? 1 : -1;
      src1.a[m] = (__bf16) (sign * (1.5 * (1 << (m % 3))));
      sign = m % 5 ? 1 : -1;
      src2.a[m] = (__bf16) (sign * (2.5 * (1 << (m % 3))));
    }

  for (m = 0; m < 1024; m++)
    dst_ref.buf[m] = 0;

  calc_op2bf16ps (&dst_ref, src1.a, src2.a);

  init_tile_config (&cfg, &bsr0);
  _tile_ace_zero (1);
  _tile_op2bf16_ps (1, src1.x, src2.x);

  CHECK_TILE_REGISTER (1, dst_ref);
}
