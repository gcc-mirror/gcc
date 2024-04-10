/* { dg-options "-mlsx -w -fno-strict-aliasing" } */
/* { dg-timeout 500 } */
#include "../simd_correctness_check.h"
#include <lsxintrin.h>
int
main ()
{
  __m128i __m128i_op0, __m128i_op1, __m128i_op2, __m128i_out, __m128i_result;
  __m128 __m128_op0, __m128_op1, __m128_op2, __m128_out, __m128_result;
  __m128d __m128d_op0, __m128d_op1, __m128d_op2, __m128d_out, __m128d_result;

  int int_op0, int_op1, int_op2, int_out, int_result, i = 1, fail;
  long int long_op0, long_op1, long_op2, lont_out, lont_result;
  long int long_int_out, long_int_result;
  unsigned int unsigned_int_out, unsigned_int_result;
  unsigned long int unsigned_long_int_out, unsigned_long_int_result;

  *((int *)&__m128_op0[3]) = 0x00100010;
  *((int *)&__m128_op0[2]) = 0x00030000;
  *((int *)&__m128_op0[1]) = 0x00060002;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrint_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrint_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000001;
  *((int *)&__m128_op0[2]) = 0xca02f854;
  *((int *)&__m128_op0[1]) = 0x00000001;
  *((int *)&__m128_op0[0]) = 0x00013fa0;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0xca02f854;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrint_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x000000ad;
  *((int *)&__m128_op0[2]) = 0x00007081;
  *((int *)&__m128_op0[1]) = 0x00000351;
  *((int *)&__m128_op0[0]) = 0x0000b5f2;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrint_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00ff00ef;
  *((int *)&__m128_op0[2]) = 0x00ff010f;
  *((int *)&__m128_op0[1]) = 0x00ff00ff;
  *((int *)&__m128_op0[0]) = 0x00ff010f;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrint_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrne_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00130013;
  *((int *)&__m128_op0[2]) = 0x00130013;
  *((int *)&__m128_op0[1]) = 0x00130013;
  *((int *)&__m128_op0[0]) = 0x00130013;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrne_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x20202020;
  *((int *)&__m128_op0[2]) = 0x20202020;
  *((int *)&__m128_op0[1]) = 0x20202020;
  *((int *)&__m128_op0[0]) = 0x20207fff;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrne_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x01f50000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrne_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_result[3]) = 0xffffffff;
  *((int *)&__m128_result[2]) = 0xffffffff;
  *((int *)&__m128_result[1]) = 0xffffffff;
  *((int *)&__m128_result[0]) = 0xffffffff;
  __m128_out = __lsx_vfrintrne_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000001;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000001;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrne_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00020004;
  *((int *)&__m128_op0[0]) = 0x00000001;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrne_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xfffbfffb;
  *((int *)&__m128_op0[2]) = 0xfffbfffb;
  *((int *)&__m128_op0[1]) = 0xfffbfffb;
  *((int *)&__m128_op0[0]) = 0xfffbfffb;
  *((int *)&__m128_result[3]) = 0xfffbfffb;
  *((int *)&__m128_result[2]) = 0xfffbfffb;
  *((int *)&__m128_result[1]) = 0xfffbfffb;
  *((int *)&__m128_result[0]) = 0xfffbfffb;
  __m128_out = __lsx_vfrintrne_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x0ff780a1;
  *((int *)&__m128_op0[2]) = 0x0efc01af;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0xfe7f0000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0xfe7f0000;
  __m128_out = __lsx_vfrintrne_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrp_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0xefffffff;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0xefffffff;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrp_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffff00;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffff00;
  *((int *)&__m128_result[3]) = 0xffffffff;
  *((int *)&__m128_result[2]) = 0xffffff00;
  *((int *)&__m128_result[1]) = 0xffffffff;
  *((int *)&__m128_result[0]) = 0xffffff00;
  __m128_out = __lsx_vfrintrp_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffb96b;
  *((int *)&__m128_op0[2]) = 0xffff57c9;
  *((int *)&__m128_op0[1]) = 0xffff6080;
  *((int *)&__m128_op0[0]) = 0xffff4417;
  *((int *)&__m128_result[3]) = 0xffffb96b;
  *((int *)&__m128_result[2]) = 0xffff57c9;
  *((int *)&__m128_result[1]) = 0xffff6080;
  *((int *)&__m128_result[0]) = 0xffff4417;
  __m128_out = __lsx_vfrintrp_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00ff00ff;
  *((int *)&__m128_op0[2]) = 0x00ff00ff;
  *((int *)&__m128_op0[1]) = 0x62cbf96e;
  *((int *)&__m128_op0[0]) = 0x4acfaf40;
  *((int *)&__m128_result[3]) = 0x3f800000;
  *((int *)&__m128_result[2]) = 0x3f800000;
  *((int *)&__m128_result[1]) = 0x62cbf96e;
  *((int *)&__m128_result[0]) = 0x4acfaf40;
  __m128_out = __lsx_vfrintrp_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00002000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x1fe02000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x3f800000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x3f800000;
  __m128_out = __lsx_vfrintrp_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_result[3]) = 0xffffffff;
  *((int *)&__m128_result[2]) = 0xffffffff;
  *((int *)&__m128_result[1]) = 0xffffffff;
  *((int *)&__m128_result[0]) = 0xffffffff;
  __m128_out = __lsx_vfrintrp_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x63636363;
  *((int *)&__m128_op0[2]) = 0x63abdf16;
  *((int *)&__m128_op0[1]) = 0x41f8e080;
  *((int *)&__m128_op0[0]) = 0x16161198;
  *((int *)&__m128_result[3]) = 0x63636363;
  *((int *)&__m128_result[2]) = 0x63abdf16;
  *((int *)&__m128_result[1]) = 0x42000000;
  *((int *)&__m128_result[0]) = 0x3f800000;
  __m128_out = __lsx_vfrintrp_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrm_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xa5c4c774;
  *((int *)&__m128_op0[2]) = 0x856ba83b;
  *((int *)&__m128_op0[1]) = 0x8003caef;
  *((int *)&__m128_op0[0]) = 0x54691124;
  *((int *)&__m128_result[3]) = 0xbf800000;
  *((int *)&__m128_result[2]) = 0xbf800000;
  *((int *)&__m128_result[1]) = 0xbf800000;
  *((int *)&__m128_result[0]) = 0x54691124;
  __m128_out = __lsx_vfrintrm_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00010002;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xff960015;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0xffffffff;
  *((int *)&__m128_result[0]) = 0xffd60015;
  __m128_out = __lsx_vfrintrm_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0x3c992b2e;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffff730f;
  *((int *)&__m128_result[3]) = 0xffffffff;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0xffffffff;
  *((int *)&__m128_result[0]) = 0xffff730f;
  __m128_out = __lsx_vfrintrz_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000001;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000016;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrz_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x18171615;
  *((int *)&__m128_op0[2]) = 0x17161514;
  *((int *)&__m128_op0[1]) = 0x16151413;
  *((int *)&__m128_op0[0]) = 0x15141312;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrz_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x62cbf96e;
  *((int *)&__m128_op0[2]) = 0x4acfaf40;
  *((int *)&__m128_op0[1]) = 0xf0bc9a52;
  *((int *)&__m128_op0[0]) = 0x78285a4a;
  *((int *)&__m128_result[3]) = 0x62cbf96e;
  *((int *)&__m128_result[2]) = 0x4acfaf40;
  *((int *)&__m128_result[1]) = 0xf0bc9a52;
  *((int *)&__m128_result[0]) = 0x78285a4a;
  __m128_out = __lsx_vfrintrz_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfrintrz_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  return 0;
}
