/* { dg-do compile } */
/* { dg-options "-O3 -mlsx" } */

#include <stdint.h>

typedef int8_t orc_int8;
typedef int16_t orc_int16;
typedef int32_t orc_int32;
typedef int64_t orc_int64;

typedef union
{
  orc_int32 i;
  float f;
  orc_int16 x2[2];
  orc_int8 x4[4];
} orc_union32;
typedef union
{
  orc_int64 i;
  double f;
  orc_int32 x2[2];
  float x2f[2];
  orc_int16 x4[4];
} orc_union64;

void
audio_orc_s32_to_double (double * restrict d1,
    const signed int * restrict s1, int n)
{
  int i;
  orc_union64 *restrict ptr0;
  const orc_union32 *restrict ptr4;
  orc_union32 var33;
  orc_union64 var34;
  orc_union64 var35;
  orc_union64 var36;

  ptr0 = (orc_union64 *) d1;
  ptr4 = (orc_union32 *) s1;

  var34.i = 0x41e0000000000000UL;

  for (i = 0; i < n; i++) {
    var33 = ptr4[i];
    var36.f = var33.i;
    {
      orc_union64 _src1;
      orc_union64 _src2;
      orc_union64 _dest1;
      _src1.i = ((var36.i) & ((((var36.i)&0x7ff0000000000000UL) == 0) ? 0xfff0000000000000UL : 0xffffffffffffffffUL));
      _src2.i = ((var34.i) & ((((var34.i)&0x7ff0000000000000UL) == 0) ? 0xfff0000000000000UL : 0xffffffffffffffffUL));
      _dest1.f = _src1.f / _src2.f;
      var35.i = ((_dest1.i) & ((((_dest1.i)&0x7ff0000000000000UL) == 0) ? 0xfff0000000000000UL : 0xffffffffffffffffUL));
    }
    ptr0[i] = var35;
  }
}
