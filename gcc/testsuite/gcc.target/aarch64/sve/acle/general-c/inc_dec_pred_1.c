#include <arm_sve.h>

void
test (svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svint32_t s32, svuint32_t u32,
      svint64_t s64, svuint64_t u64, int i)
{
  svqincp (s32); /* { dg-error {too few arguments to function 'svqincp'} } */
  svqincp (s32, pg, pg); /* { dg-error {too many arguments to function 'svqincp'} } */
  svqincp (i, pg); /* { dg-error {passing 'int' to argument 1 of 'svqincp', which expects an SVE type rather than a scalar} } */
  svqincp (pg, pg); /* { dg-error {'svqincp' has no form that takes 'svbool_t' arguments} } */
  svqincp (s8, pg); /* { dg-error {'svqincp' has no form that takes 'svint8_t' arguments} } */
  svqincp (u8, pg); /* { dg-error {'svqincp' has no form that takes 'svuint8_t' arguments} } */
  svqincp (s16, pg);
  svqincp (u16, pg);
  svqincp (s32, pg);
  svqincp (u32, pg);
  svqincp (s64, pg);
  svqincp (u64, pg);
  svqincp (u64, 0); /* { dg-error {passing 'int' to argument 2 of 'svqincp', which expects 'svbool_t'} } */
  svqincp (u64, u64); /* { dg-error {passing 'svuint64_t' to argument 2 of 'svqincp', which expects 'svbool_t'} } */
}
