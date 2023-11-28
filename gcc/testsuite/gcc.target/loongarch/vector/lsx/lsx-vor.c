/* { dg-options "-mlsx -w -fno-strict-aliasing" } */
#include "../simd_correctness_check.h"
#include <lsxintrin.h>

int main ()
{
  __m128i __m128i_op0, __m128i_op1, __m128i_op2, __m128i_out, __m128i_result;
  __m128 __m128_op0, __m128_op1, __m128_op2, __m128_out, __m128_result;
  __m128d __m128d_op0, __m128d_op1, __m128d_op2, __m128d_out, __m128d_result;

  int int_op0, int_op1, int_op2, int_out, int_result, i=1, fail;
  long int long_op0, long_op1, long_op2, lont_out, lont_result;
  long int long_int_out, long_int_result;
  unsigned int unsigned_int_out, unsigned_int_result;
  unsigned long int unsigned_long_int_out, unsigned_long_int_result;

  *((unsigned long*)& __m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_op1[1]) = 0x7e44bde9b842ff23;
  *((unsigned long*)& __m128i_op1[0]) = 0x00011e80007edff8;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long*)& __m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long*)& __m128i_op1[0]) = 0xfffc001fffffffff;
  *((unsigned long*)& __m128i_result[1]) = 0x00000000ffffffff;
  *((unsigned long*)& __m128i_result[0]) = 0xfffc001fffffffff;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000200010;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000200010;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000200010;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x4f804f804f804f80;
  *((unsigned long*)& __m128i_op0[0]) = 0x4f804f804f804f80;
  *((unsigned long*)& __m128i_op1[1]) = 0x4f804f804f804f80;
  *((unsigned long*)& __m128i_op1[0]) = 0x4f804f804f804f80;
  *((unsigned long*)& __m128i_result[1]) = 0x4f804f804f804f80;
  *((unsigned long*)& __m128i_result[0]) = 0x4f804f804f804f80;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x3e035e51522f0799;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x3e035e51522f0799;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x3e035e51522f0799;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffff0000ffff0000;
  *((unsigned long*)& __m128i_op0[0]) = 0xffff000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0xffff0000ffff0000;
  *((unsigned long*)& __m128i_op1[0]) = 0xffff000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xffff0000ffff0000;
  *((unsigned long*)& __m128i_result[0]) = 0xffff000000000000;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0xffffffff00000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xfffb00fdfdf7ffff;
  *((unsigned long*)& __m128i_op0[0]) = 0xfff8000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0xfffb00fdfdf7ffff;
  *((unsigned long*)& __m128i_op1[0]) = 0xfff8000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0xfffb00fdfdf7ffff;
  *((unsigned long*)& __m128i_result[0]) = 0xfff8000000000000;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffffffff80005613;
  *((unsigned long*)& __m128i_op0[0]) = 0x81000080806b000b;
  *((unsigned long*)& __m128i_op1[1]) = 0xffff00011cf0c569;
  *((unsigned long*)& __m128i_op1[0]) = 0xc0000002b0995850;
  *((unsigned long*)& __m128i_result[1]) = 0xffffffff9cf0d77b;
  *((unsigned long*)& __m128i_result[0]) = 0xc1000082b0fb585b;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0xfffffffffffbfff8;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x80808080806b000b;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0xfffffffffffbfffb;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0001000101010001;
  *((unsigned long*)& __m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000ffff0000ffff;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000ffff0000ffff;
  *((unsigned long*)& __m128i_result[1]) = 0x0001ffff0101ffff;
  *((unsigned long*)& __m128i_result[0]) = 0x0001ffff0001ffff;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0xffffffffc105d1aa;
  *((unsigned long*)& __m128i_op0[0]) = 0xffffffffbc19ecca;
  *((unsigned long*)& __m128i_op1[1]) = 0xc0b4d1a5f8babad3;
  *((unsigned long*)& __m128i_op1[0]) = 0xbbc8ecc5f3ced5f3;
  *((unsigned long*)& __m128i_result[1]) = 0xfffffffff9bffbfb;
  *((unsigned long*)& __m128i_result[0]) = 0xffffffffffdffdfb;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  *((unsigned long*)& __m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vor_v(__m128i_op0,__m128i_op1);
  ASSERTEQ_64(__LINE__, __m128i_result, __m128i_out);


  return 0;
}
