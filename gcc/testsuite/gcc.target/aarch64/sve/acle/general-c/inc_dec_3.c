#include <arm_sve.h>

void
test (svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
      svint64_t s64, svuint64_t u64, int16_t sh, uint16_t uh,
      int32_t sw, uint32_t uw, int64_t sd, uint64_t ud,
      float f)
{
  svqinch (pg, 1); /* { dg-error {'svqinch' has no form that takes 'svbool_t' arguments} } */
  svqinch (s8, 1); /* { dg-error {'svqinch' has no form that takes 'svint8_t' arguments} } */
  svqinch (u8, 1); /* { dg-error {'svqinch' has no form that takes 'svuint8_t' arguments} } */
  svqinch (s16, 1);
  svqinch (u16, 1);
  svqinch (s32, 1); /* { dg-error {'svqinch' has no form that takes 'svint32_t' arguments} } */
  svqinch (u32, 1); /* { dg-error {'svqinch' has no form that takes 'svuint32_t' arguments} } */
  svqinch (s64, 1); /* { dg-error {'svqinch' has no form that takes 'svint64_t' arguments} } */
  svqinch (u64, 1); /* { dg-error {'svqinch' has no form that takes 'svuint64_t' arguments} } */
  svqinch (sh, 1);
  svqinch (sw, 1);
  svqinch (sd, 1);
  svqinch (uh, 1);
  svqinch (uw, 1);
  svqinch (ud, 1);
  svqinch (f, 1); /* { dg-error {passing 'float' to argument 1 of 'svqinch', which expects a 32-bit or 64-bit integer type} } */
}
