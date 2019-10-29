#include <arm_sve.h>

void
test (svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
      svint64_t s64, svuint64_t u64, int16_t sh, uint16_t uh,
      int32_t sw, uint32_t uw, int64_t sd, uint64_t ud,
      float f)
{
  svqincw (pg, 1); /* { dg-error {'svqincw' has no form that takes 'svbool_t' arguments} } */
  svqincw (s8, 1); /* { dg-error {'svqincw' has no form that takes 'svint8_t' arguments} } */
  svqincw (u8, 1); /* { dg-error {'svqincw' has no form that takes 'svuint8_t' arguments} } */
  svqincw (s16, 1); /* { dg-error {'svqincw' has no form that takes 'svint16_t' arguments} } */
  svqincw (u16, 1); /* { dg-error {'svqincw' has no form that takes 'svuint16_t' arguments} } */
  svqincw (s32, 1);
  svqincw (u32, 1);
  svqincw (s64, 1); /* { dg-error {'svqincw' has no form that takes 'svint64_t' arguments} } */
  svqincw (u64, 1); /* { dg-error {'svqincw' has no form that takes 'svuint64_t' arguments} } */
  svqincw (sh, 1);
  svqincw (sw, 1);
  svqincw (sd, 1);
  svqincw (uh, 1);
  svqincw (uw, 1);
  svqincw (ud, 1);
  svqincw (f, 1); /* { dg-error {passing 'float' to argument 1 of 'svqincw', which expects a 32-bit or 64-bit integer type} } */
}
