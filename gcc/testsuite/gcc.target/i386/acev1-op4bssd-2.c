/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target acev1 } */
/* { dg-options "-O2 -macev1" } */
#define DO_TEST test_acev1_op4bssd
void test_acev1_op4bssd ();
#include "ace-helper.h"

void calc_op4bssd (__tile *dst, char *src1, char *src2)
{
  int i, j;

  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j++)
      {
	dst->b[i * 16 + j] += (int) src1[4 * i] * (int) src2[4 * j];
	dst->b[i * 16 + j] += (int) src1[4 * i + 1] * (int) src2[4 * j + 1];
	dst->b[i * 16 + j] += (int) src1[4 * i + 2] * (int) src2[4 * j + 2];
	dst->b[i * 16 + j] += (int) src1[4 * i + 3] * (int) src2[4 * j + 3];
      }
}

void test_acev1_op4bssd ()
{
  __tilecfg cfg;
  __tile dst_ref;
  __bsr bsr0;
  union512i_b src1, src2;
  int m;

  for (m = 0; m < 64; m++)
    {
      int sign = m % 2 ? 1 : -1;
      src1.a[m] = 10 + 3 * m + sign;
      src2.a[m] = sign * 10 * m * m;
    }

  for (m = 0; m < 1024; m++)
    dst_ref.buf[m] = 0;

  calc_op4bssd (&dst_ref, src1.a, src2.a);

  init_tile_config (&cfg, &bsr0);
  _tile_ace_zero (1);
  _tile_op4bssd_epi32 (1, src1.x, src2.x);

  CHECK_TILE_REGISTER (1, dst_ref); 
}
