#include <arm_sve.h>

void
test (enum svpattern pat, svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
      svint64_t s64, svuint64_t u64, int16_t sh, uint16_t uh,
      int32_t sw, uint32_t uw, int64_t sd, uint64_t ud,
      float f, int i)
{
  svqincb_pat (sw, pat); /* { dg-error {too few arguments to function 'svqincb_pat'} } */
  svqincb_pat (sw, pat, 1, 1); /* { dg-error {too many arguments to function 'svqincb_pat'} } */

  svqincb_pat (pg, SV_ALL, 1); /* { dg-error {'svqincb_pat' has no form that takes 'svbool_t' arguments} } */
  svqincb_pat (s8, SV_ALL, 1); /* { dg-error {'svqincb_pat' has no form that takes 'svint8_t' arguments} } */
  svqincb_pat (u8, SV_ALL, 1); /* { dg-error {'svqincb_pat' has no form that takes 'svuint8_t' arguments} } */
  svqincb_pat (s16, SV_ALL, 1); /* { dg-error {'svqincb_pat' has no form that takes 'svint16_t' arguments} } */
  svqincb_pat (u16, SV_ALL, 1); /* { dg-error {'svqincb_pat' has no form that takes 'svuint16_t' arguments} } */
  svqincb_pat (s32, SV_ALL, 1); /* { dg-error {'svqincb_pat' has no form that takes 'svint32_t' arguments} } */
  svqincb_pat (u32, SV_ALL, 1); /* { dg-error {'svqincb_pat' has no form that takes 'svuint32_t' arguments} } */
  svqincb_pat (s64, SV_ALL, 1); /* { dg-error {'svqincb_pat' has no form that takes 'svint64_t' arguments} } */
  svqincb_pat (u64, SV_ALL, 1); /* { dg-error {'svqincb_pat' has no form that takes 'svuint64_t' arguments} } */
  svqincb_pat (sh, SV_ALL, 1);
  svqincb_pat (sw, SV_ALL, 1);
  svqincb_pat (sd, SV_ALL, 1);
  svqincb_pat (uh, SV_ALL, 1);
  svqincb_pat (uw, SV_ALL, 1);
  svqincb_pat (ud, SV_ALL, 1);
  svqincb_pat (f, SV_ALL, 1); /* { dg-error {passing 'float' to argument 1 of 'svqincb_pat', which expects a 32-bit or 64-bit integer type} } */

  svqincb_pat (sw, pat, 1); /* { dg-error {argument 2 of 'svqincb_pat' must be an integer constant expression} } */
  svqincb_pat (sw, i, 1); /* { dg-error {argument 2 of 'svqincb_pat' must be an integer constant expression} } */
  svqincb_pat (sw, (enum svpattern) -1, 1); /* { dg-error {passing 4294967295 to argument 2 of 'svqincb_pat', which expects a valid 'enum svpattern' value} } */
  svqincb_pat (sw, (enum svpattern) 0, 1);
  svqincb_pat (sw, (enum svpattern) 13, 1);
  svqincb_pat (sw, (enum svpattern) 14, 1); /* { dg-error {passing 14 to argument 2 of 'svqincb_pat', which expects a valid 'enum svpattern' value} } */
  svqincb_pat (sw, (enum svpattern) 28, 1); /* { dg-error {passing 28 to argument 2 of 'svqincb_pat', which expects a valid 'enum svpattern' value} } */
  svqincb_pat (sw, (enum svpattern) 29, 1);
  svqincb_pat (sw, (enum svpattern) 31, 1);
  svqincb_pat (sw, (enum svpattern) 32, 1); /* { dg-error {passing 32 to argument 2 of 'svqincb_pat', which expects a valid 'enum svpattern' value} } */

  svqincb_pat (sw, SV_POW2, -1); /* { dg-error {passing -1 to argument 3 of 'svqincb_pat', which expects a value in the range \[1, 16\]} } */
  svqincb_pat (sw, SV_POW2, 0); /* { dg-error {passing 0 to argument 3 of 'svqincb_pat', which expects a value in the range \[1, 16\]} } */
  svqincb_pat (sw, SV_POW2, 1);
  svqincb_pat (sw, SV_POW2, 2);
  svqincb_pat (sw, SV_POW2, 16);
  svqincb_pat (sw, SV_POW2, 17); /* { dg-error {passing 17 to argument 3 of 'svqincb_pat', which expects a value in the range \[1, 16\]} } */
}
