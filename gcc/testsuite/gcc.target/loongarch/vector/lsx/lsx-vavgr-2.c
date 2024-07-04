/* { dg-options "-mlsx -w -fno-strict-aliasing" } */
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

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffff8f8dada;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff01018888;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffff8f8dada;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff01018888;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffff8f8dada;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff01018888;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x4080808080808080;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000010000003f;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000010000003f;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[0]) = 0x000000010000003f;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffffffff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffff00;
  *((unsigned long *)&__m128i_op1[1]) = 0x1817161517161514;
  *((unsigned long *)&__m128i_op1[0]) = 0x1615141315141312;
  *((unsigned long *)&__m128i_result[1]) = 0x0c0c8b8a8b8b0b0a;
  *((unsigned long *)&__m128i_result[0]) = 0x8b8a8a898a8a8909;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00003fff00003fff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00003fff00003fff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_result[0]) = 0x0000208000002080;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffd60015;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x80808080806b000b;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fff010181010102;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fffffff81010102;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fff010181010102;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fffffff81010102;
  *((unsigned long *)&__m128i_result[1]) = 0x7fff010181010102;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffff81010102;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xc1bdceee242071db;
  *((unsigned long *)&__m128i_op1[0]) = 0xe8c7b756d76aa578;
  *((unsigned long *)&__m128i_result[1]) = 0xe0dee7779210b8ed;
  *((unsigned long *)&__m128i_result[0]) = 0xf463dbabebb5d2bc;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xff80000000000000;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000800000008;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00007fff00007fff;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000002bfd9461;
  *((unsigned long *)&__m128i_result[1]) = 0x0000400400004004;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000015ff4a31;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x2a7b7c9260f90ee2;
  *((unsigned long *)&__m128i_op1[0]) = 0x1b1c6cdfd57f5736;
  *((unsigned long *)&__m128i_result[1]) = 0x153e3e49307d0771;
  *((unsigned long *)&__m128i_result[0]) = 0x0d8e36706ac02b9b;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xdd6156076967d8c9;
  *((unsigned long *)&__m128i_op0[0]) = 0x2e3ab5266375e71b;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000400000004;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000400000004;
  *((unsigned long *)&__m128i_result[1]) = 0x6eb12b0634b46c67;
  *((unsigned long *)&__m128i_result[0]) = 0x171d5a9531bb7390;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000f50000000900;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000090900000998;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00007a8000000480;
  *((unsigned long *)&__m128i_result[0]) = 0x00000485000004cc;
  __m128i_out = __lsx_vavgr_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00003ff000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000fffc00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffc001fffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x00001ff800000001;
  *((unsigned long *)&__m128i_result[0]) = 0x7ffe800e80000000;
  __m128i_out = __lsx_vavgr_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffff53d9;
  *((unsigned long *)&__m128i_op0[0]) = 0xff000001ffff9515;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x000000007fffa9ed;
  *((unsigned long *)&__m128i_result[0]) = 0x7f8000017fffca8b;
  __m128i_out = __lsx_vavgr_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffdfffffff8;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7fffffff7ffffffc;
  __m128i_out = __lsx_vavgr_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000001fffeff98;
  *((unsigned long *)&__m128i_op0[0]) = 0x0014ffe4ff76ffc4;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x3131313131313131;
  *((unsigned long *)&__m128i_result[1]) = 0x000000017fff7fcc;
  *((unsigned long *)&__m128i_result[0]) = 0x18a3188b9854187b;
  __m128i_out = __lsx_vavgr_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_result[0]) = 0xffff000000000000;
  __m128i_out = __lsx_vavgr_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000001c88bf0;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000001c88bf0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000001c88bf0;
  __m128i_out = __lsx_vavgr_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x807fffff80800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0006000000040000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0002000000000007;
  *((unsigned long *)&__m128i_result[1]) = 0x8003000000020000;
  *((unsigned long *)&__m128i_result[0]) = 0x4040ffffc0400004;
  __m128i_out = __lsx_vavgr_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000010000010101;
  *((unsigned long *)&__m128i_op0[0]) = 0x0101000001000100;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x8000008000008080;
  *((unsigned long *)&__m128i_result[0]) = 0x8080800000800080;
  __m128i_out = __lsx_vavgr_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}
