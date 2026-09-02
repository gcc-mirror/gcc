/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target acev1 } */
/* { dg-options "-O2 -macev1" } */
#define DO_TEST test_acev1_op4mxhf8ps
void test_acev1_op4mxhf8ps ();
#include "ace-helper.h"

void calc_op4mxhf8ps (__tile *dst, char *src1, char *src2, __bsr* bsr, const int imm)
{
  int i, j;

  for (i = 0; i < 16; i++)
    {
      for (j = 0; j < 16; j++)
	{
	  __int128_t tmp = 0;
	  float scale = convert_e8m0_to_fp32 (bsr->buf[4 * i  + (imm & 0x3)])
	    * convert_e8m0_to_fp32 (bsr->buf[64 + 4 * j + ((imm & 0x18) >> 3)]);
	  tmp += (__int128_t) shift_fp8_to_int64 (src1[4 * i], 0, NULL)
	    * (__int128_t) shift_fp8_to_int64 (src2[4 * j], 0, NULL);
	  tmp += (__int128_t) shift_fp8_to_int64 (src1[4 * i + 1], 0, NULL)
	    * (__int128_t) shift_fp8_to_int64 (src2[4 * j + 1], 0, NULL);
	  tmp += (__int128_t) shift_fp8_to_int64 (src1[4 * i + 2], 0, NULL)
	    * (__int128_t) shift_fp8_to_int64 (src2[4 * j + 2], 0, NULL);
	  tmp += (__int128_t) shift_fp8_to_int64 (src1[4 * i + 3], 0, NULL)
	    * (__int128_t) shift_fp8_to_int64 (src2[4 * j + 3], 0, NULL);
	  dst->a[16 * i + j] += shift_int128_to_fp32 (tmp, 0, 0) * scale;
	}
    }
}

void test_acev1_op4mxhf8ps ()
{
  __tilecfg cfg;
  __tile dst_ref;
  __bsr bsr0;
  union512i_b src1, src2;
  union512i_ub bsrl, bsrh;
  int m;

  for (m = 0; m < 64; m++)
    {
      src1.a[m] = (m % 2) << 7 | (m % 16) << 3 | ((m + 1) % 8);
      src2.a[m] = ((m % 3) % 2) << 7 | ((m + 7) % 16) << 3 | ((m + 2) % 8);
    }

  for (m = 0; m < 1024; m++)
    dst_ref.buf[m] = 0;

  init_tile_config (&cfg, &bsr0);
  fill_bsr (&bsr0, &bsrl, &bsrh);
  calc_op4mxhf8ps (&dst_ref, src1.a, src2.a, &bsr0, 16);

  _tile_ace_zero (1);
  _bsr0_insertfull (bsrh.x, bsrl.x);
  _tile_op4mxhf8_ps (1, src1.x, src2.x, 16);

  CHECK_TILE_REGISTER (1, dst_ref); 
}
