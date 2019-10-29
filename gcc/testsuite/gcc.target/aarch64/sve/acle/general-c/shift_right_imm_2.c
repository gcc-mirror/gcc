/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svint16_t s16, svint32_t s32, svint64_t s64,
    int x)
{
  const int one = 1;
  s8 = svasrd_n_s8_x (pg, s8, x); /* { dg-error {argument 3 of 'svasrd_n_s8_x' must be an integer constant expression} } */
  s8 = svasrd_n_s8_x (pg, s8, one); /* { dg-error {argument 3 of 'svasrd_n_s8_x' must be an integer constant expression} } */
  s8 = svasrd_n_s8_x (pg, s8, 0.4); /* { dg-error {passing 0 to argument 3 of 'svasrd_n_s8_x', which expects a value in the range \[1, 8\]} } */
  s8 = svasrd_n_s8_x (pg, s8, 1.0);
  s8 = svasrd_n_s8_x (pg, s8, 0); /* { dg-error {passing 0 to argument 3 of 'svasrd_n_s8_x', which expects a value in the range \[1, 8\]} } */
  s8 = svasrd_n_s8_x (pg, s8, 1);
  s8 = svasrd_n_s8_x (pg, s8, 1 + 1);
  s8 = svasrd_n_s8_x (pg, s8, 8);
  s8 = svasrd_n_s8_x (pg, s8, 9); /* { dg-error {passing 9 to argument 3 of 'svasrd_n_s8_x', which expects a value in the range \[1, 8\]} } */
  s8 = svasrd_n_s8_x (pg, s8, (1ULL << 62) + 1); /* { dg-error {passing [^ ]* to argument 3 of 'svasrd_n_s8_x', which expects a value in the range \[1, 8\]} } */
  s16 = svasrd_n_s16_x (pg, s16, 0); /* { dg-error {passing 0 to argument 3 of 'svasrd_n_s16_x', which expects a value in the range \[1, 16\]} } */
  s16 = svasrd_n_s16_x (pg, s16, 1);
  s16 = svasrd_n_s16_x (pg, s16, 16);
  s16 = svasrd_n_s16_x (pg, s16, 17); /* { dg-error {passing 17 to argument 3 of 'svasrd_n_s16_x', which expects a value in the range \[1, 16\]} } */
  s32 = svasrd_n_s32_x (pg, s32, 0); /* { dg-error {passing 0 to argument 3 of 'svasrd_n_s32_x', which expects a value in the range \[1, 32\]} } */
  s32 = svasrd_n_s32_x (pg, s32, 1);
  s32 = svasrd_n_s32_x (pg, s32, 32);
  s32 = svasrd_n_s32_x (pg, s32, 33); /* { dg-error {passing 33 to argument 3 of 'svasrd_n_s32_x', which expects a value in the range \[1, 32\]} } */
  s64 = svasrd_n_s64_x (pg, s64, 0); /* { dg-error {passing 0 to argument 3 of 'svasrd_n_s64_x', which expects a value in the range \[1, 64\]} } */
  s64 = svasrd_n_s64_x (pg, s64, 1);
  s64 = svasrd_n_s64_x (pg, s64, 64);
  s64 = svasrd_n_s64_x (pg, s64, 65); /* { dg-error {passing 65 to argument 3 of 'svasrd_n_s64_x', which expects a value in the range \[1, 64\]} } */
}
