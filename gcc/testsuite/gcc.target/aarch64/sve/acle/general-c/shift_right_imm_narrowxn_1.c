/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("+sme2")

void
f1 (svboolx2_t pgx2,
    svint8x2_t s8x2, svuint8x2_t u8x2,
    svint8x4_t s8x4, svuint8x4_t u8x4,
    svint16x2_t s16x2, svuint16x2_t u16x2,
    svint16x4_t s16x4, svuint16x4_t u16x4,
    svint32x2_t s32x2, svuint32x2_t u32x2,
    svint32x4_t s32x4, svuint32x4_t u32x4,
    svint64x2_t s64x2, svuint64x2_t u64x2,
    svint64x4_t s64x4, svuint64x4_t u64x4,
    svfloat32x2_t f32x2, int x) __arm_streaming
{
  const int one = 1;
  svqrshr_u8 (u32x4); /* { dg-error {too few arguments to function 'svqrshr_u8'} } */
  svqrshr_u8 (u32x4, 1, 1); /* { dg-error {too many arguments to function 'svqrshr_u8'} } */

  svqrshr_u8 (u32x4, x); /* { dg-error {argument 2 of 'svqrshr_u8' must be an integer constant expression} } */
  svqrshr_u8 (u32x4, one); /* { dg-error {argument 2 of 'svqrshr_u8' must be an integer constant expression} } */
  svqrshr_u8 (u32x4, 0.4); /* { dg-error {passing 0 to argument 2 of 'svqrshr_u8', which expects a value in the range \[1, 32\]} } */
  svqrshr_u8 (u32x4, 1.0);

  svqrshr_u8 (pgx2, 1); /* { dg-error {'svqrshr_u8' has no form that takes 'svboolx2_t' arguments} } */
  svqrshr_u8 (u8x2, 1); /* { dg-error {'svqrshr_u8' has no form that takes 'svuint8x2_t' arguments} } */
  svqrshr_u8 (u8x4, 1); /* { dg-error {'svqrshr_u8' has no form that takes 'svuint8x4_t' arguments} } */
  svqrshr_u8 (u16x2, 1); /* { dg-error {'svqrshr_u8' has no form that takes 'svuint16x2_t' arguments} } */
  svqrshr_u8 (u16x4, 1); /* { dg-error {'svqrshr_u8' has no form that takes 'svuint16x4_t' arguments} } */
  svqrshr_u8 (u32x2, 1); /* { dg-error {'svqrshr_u8' has no form that takes 'svuint32x2_t' arguments} } */
  svqrshr_u8 (u32x4, 1);
  svqrshr_u8 (u64x2, 1); /* { dg-error {'svqrshr_u8' has no form that takes 'svuint64x2_t' arguments} } */
  svqrshr_u8 (u64x4, 1); /* { dg-error {'svqrshr_u8' has no form that takes 'svuint64x4_t' arguments} } */
  svqrshr_u8 (s32x4, 1); /* { dg-error {'svqrshr_u8' has no form that takes 'svint32x4_t' arguments} } */

  svqrshr_s8 (s8x2, 1); /* { dg-error {'svqrshr_s8' has no form that takes 'svint8x2_t' arguments} } */
  svqrshr_s8 (s8x4, 1); /* { dg-error {'svqrshr_s8' has no form that takes 'svint8x4_t' arguments} } */
  svqrshr_s8 (s16x2, 1); /* { dg-error {'svqrshr_s8' has no form that takes 'svint16x2_t' arguments} } */
  svqrshr_s8 (s16x4, 1); /* { dg-error {'svqrshr_s8' has no form that takes 'svint16x4_t' arguments} } */
  svqrshr_s8 (s32x2, 1); /* { dg-error {'svqrshr_s8' has no form that takes 'svint32x2_t' arguments} } */
  svqrshr_s8 (s32x4, 1);
  svqrshr_s8 (s64x2, 1); /* { dg-error {'svqrshr_s8' has no form that takes 'svint64x2_t' arguments} } */
  svqrshr_s8 (s64x4, 1); /* { dg-error {'svqrshr_s8' has no form that takes 'svint64x4_t' arguments} } */
  svqrshr_s8 (u32x4, 1); /* { dg-error {'svqrshr_s8' has no form that takes 'svuint32x4_t' arguments} } */

  svqrshr_u16 (pgx2, 1); /* { dg-error {'svqrshr_u16' has no form that takes 'svboolx2_t' arguments} } */
  svqrshr_u16 (u8x2, 1); /* { dg-error {'svqrshr_u16' has no form that takes 'svuint8x2_t' arguments} } */
  svqrshr_u16 (u8x4, 1); /* { dg-error {'svqrshr_u16' has no form that takes 'svuint8x4_t' arguments} } */
  svqrshr_u16 (u16x2, 1); /* { dg-error {'svqrshr_u16' has no form that takes 'svuint16x2_t' arguments} } */
  svqrshr_u16 (u16x4, 1); /* { dg-error {'svqrshr_u16' has no form that takes 'svuint16x4_t' arguments} } */
  svqrshr_u16 (u32x2, 1);
  svqrshr_u16 (u32x4, 1); /* { dg-error {'svqrshr_u16' has no form that takes 'svuint32x4_t' arguments} } */
  svqrshr_u16 (u64x2, 1); /* { dg-error {'svqrshr_u16' has no form that takes 'svuint64x2_t' arguments} } */
  svqrshr_u16 (u64x4, 1);
  svqrshr_u16 (s32x2, 1); /* { dg-error {'svqrshr_u16' has no form that takes 'svint32x2_t' arguments} } */

  svqrshr_s16 (s8x2, 1); /* { dg-error {'svqrshr_s16' has no form that takes 'svint8x2_t' arguments} } */
  svqrshr_s16 (s8x4, 1); /* { dg-error {'svqrshr_s16' has no form that takes 'svint8x4_t' arguments} } */
  svqrshr_s16 (s16x2, 1); /* { dg-error {'svqrshr_s16' has no form that takes 'svint16x2_t' arguments} } */
  svqrshr_s16 (s16x4, 1); /* { dg-error {'svqrshr_s16' has no form that takes 'svint16x4_t' arguments} } */
  svqrshr_s16 (s32x2, 1);
  svqrshr_s16 (s32x4, 1); /* { dg-error {'svqrshr_s16' has no form that takes 'svint32x4_t' arguments} } */
  svqrshr_s16 (s64x2, 1); /* { dg-error {'svqrshr_s16' has no form that takes 'svint64x2_t' arguments} } */
  svqrshr_s16 (s64x4, 1);
  svqrshr_s16 (u32x2, 1); /* { dg-error {'svqrshr_s16' has no form that takes 'svuint32x2_t' arguments} } */

  svqrshr_u8 (u32x4, -1); /* { dg-error {passing -1 to argument 2 of 'svqrshr_u8', which expects a value in the range \[1, 32\]} } */
  svqrshr_u8 (u32x4, 0); /* { dg-error {passing 0 to argument 2 of 'svqrshr_u8', which expects a value in the range \[1, 32\]} } */
  svqrshr_u8 (u32x4, 1);
  svqrshr_u8 (u32x4, 32);
  svqrshr_u8 (u32x4, 33); /* { dg-error {passing 33 to argument 2 of 'svqrshr_u8', which expects a value in the range \[1, 32\]} } */

  svqrshr_u16 (u32x2, -1); /* { dg-error {passing -1 to argument 2 of 'svqrshr_u16', which expects a value in the range \[1, 16\]} } */
  svqrshr_u16 (u32x2, 0); /* { dg-error {passing 0 to argument 2 of 'svqrshr_u16', which expects a value in the range \[1, 16\]} } */
  svqrshr_u16 (u32x2, 1);
  svqrshr_u16 (u32x2, 16);
  svqrshr_u16 (u32x2, 17); /* { dg-error {passing 17 to argument 2 of 'svqrshr_u16', which expects a value in the range \[1, 16\]} } */

  svqrshr_u16 (u64x4, -1); /* { dg-error {passing -1 to argument 2 of 'svqrshr_u16', which expects a value in the range \[1, 64\]} } */
  svqrshr_u16 (u64x4, 0); /* { dg-error {passing 0 to argument 2 of 'svqrshr_u16', which expects a value in the range \[1, 64\]} } */
  svqrshr_u16 (u64x4, 1);
  svqrshr_u16 (u64x4, 64);
  svqrshr_u16 (u64x4, 65); /* { dg-error {passing 65 to argument 2 of 'svqrshr_u16', which expects a value in the range \[1, 64\]} } */

  svqrshr_u8 (1, 1); /* { dg-error {passing 'int' to argument 1 of 'svqrshr_u8', which expects an SVE type rather than a scalar} } */
}
