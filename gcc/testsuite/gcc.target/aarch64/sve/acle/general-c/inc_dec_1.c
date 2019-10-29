#include <arm_sve.h>

void
test (svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
      svint64_t s64, svuint64_t u64, int16_t sh, uint16_t uh,
      int32_t sw, uint32_t uw, int64_t sd, uint64_t ud,
      float f, int i)
{
  svqincb (sw); /* { dg-error {too few arguments to function 'svqincb'} } */
  svqincb (sw, 1, 1); /* { dg-error {too many arguments to function 'svqincb'} } */

  svqincb (pg, 1); /* { dg-error {'svqincb' has no form that takes 'svbool_t' arguments} } */
  svqincb (s8, 1); /* { dg-error {'svqincb' has no form that takes 'svint8_t' arguments} } */
  svqincb (u8, 1); /* { dg-error {'svqincb' has no form that takes 'svuint8_t' arguments} } */
  svqincb (s16, 1); /* { dg-error {'svqincb' has no form that takes 'svint16_t' arguments} } */
  svqincb (u16, 1); /* { dg-error {'svqincb' has no form that takes 'svuint16_t' arguments} } */
  svqincb (s32, 1); /* { dg-error {'svqincb' has no form that takes 'svint32_t' arguments} } */
  svqincb (u32, 1); /* { dg-error {'svqincb' has no form that takes 'svuint32_t' arguments} } */
  svqincb (s64, 1); /* { dg-error {'svqincb' has no form that takes 'svint64_t' arguments} } */
  svqincb (u64, 1); /* { dg-error {'svqincb' has no form that takes 'svuint64_t' arguments} } */
  svqincb (sh, 1);
  svqincb (sw, 1);
  svqincb (sd, 1);
  svqincb (uh, 1);
  svqincb (uw, 1);
  svqincb (ud, 1);
  svqincb (f, 1); /* { dg-error {passing 'float' to argument 1 of 'svqincb', which expects a 32-bit or 64-bit integer type} } */
  svqincb (ud, i); /* { dg-error {argument 2 of 'svqincb' must be an integer constant expression} } */

  svqincb (sw, -1); /* { dg-error {passing -1 to argument 2 of 'svqincb', which expects a value in the range \[1, 16\]} } */
  svqincb (sw, 0); /* { dg-error {passing 0 to argument 2 of 'svqincb', which expects a value in the range \[1, 16\]} } */
  svqincb (sw, 1);
  svqincb (sw, 2);
  svqincb (sw, 16);
  svqincb (sw, 17); /* { dg-error {passing 17 to argument 2 of 'svqincb', which expects a value in the range \[1, 16\]} } */
}
