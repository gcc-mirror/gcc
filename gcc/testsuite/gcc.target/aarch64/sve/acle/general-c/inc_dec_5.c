#include <arm_sve.h>

void
test (svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
      svint64_t s64, svuint64_t u64, int16_t sh, uint16_t uh,
      int32_t sw, uint32_t uw, int64_t sd, uint64_t ud,
      float f)
{
  svqincd (pg, 1); /* { dg-error {'svqincd' has no form that takes 'svbool_t' arguments} } */
  svqincd (s8, 1); /* { dg-error {'svqincd' has no form that takes 'svint8_t' arguments} } */
  svqincd (u8, 1); /* { dg-error {'svqincd' has no form that takes 'svuint8_t' arguments} } */
  svqincd (s16, 1); /* { dg-error {'svqincd' has no form that takes 'svint16_t' arguments} } */
  svqincd (u16, 1); /* { dg-error {'svqincd' has no form that takes 'svuint16_t' arguments} } */
  svqincd (s32, 1); /* { dg-error {'svqincd' has no form that takes 'svint32_t' arguments} } */
  svqincd (u32, 1); /* { dg-error {'svqincd' has no form that takes 'svuint32_t' arguments} } */
  svqincd (s64, 1);
  svqincd (u64, 1);
  svqincd (sh, 1);
  svqincd (sw, 1);
  svqincd (sd, 1);
  svqincd (uh, 1);
  svqincd (uw, 1);
  svqincd (ud, 1);
  svqincd (f, 1); /* { dg-error {passing 'float' to argument 1 of 'svqincd', which expects a 32-bit or 64-bit integer type} } */
}
