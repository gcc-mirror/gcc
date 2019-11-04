/* { dg-do compile } */
/* { dg-additional-options "-std=c++11 -Wall -Wextra" } */

#include <arm_sve.h>

constexpr uint64_t const_add (uint64_t a, uint64_t b) { return a + b; }
uint64_t add (uint64_t a, uint64_t b) { return a + b; }

void
f1 (svbool_t pg, svuint8_t u8, svint8_t s8, svint16_t s16,
    svint32_t s32, svint64_t s64, int x)
{
  const int one = 1;
  u8 = svasrd_x (pg, u8, 1); /* { dg-error {no matching function for call to 'svasrd_x\(svbool_t&, svuint8_t&, [^)]*\)'} } */
  s8 = svasrd_x (pg, s8, x); /* { dg-error "argument 3 of 'svasrd_x' must be an integer constant expression" } */
  s8 = svasrd_x (pg, s8, one);
  s8 = svasrd_x (pg, s8, 0.4); /* { dg-error {passing 0 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 8\]} } */
  s8 = svasrd_x (pg, s8, 1.0);
  s8 = svasrd_x (pg, s8, 0); /* { dg-error {passing 0 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 8\]} } */
  s8 = svasrd_x (pg, s8, 1);
  s8 = svasrd_x (pg, s8, 1 + 1);
  s8 = svasrd_x (pg, s8, const_add (1, 1));
  s8 = svasrd_x (pg, s8, add (1, 1)); /* { dg-error "argument 3 of 'svasrd_x' must be an integer constant expression" } */
  s8 = svasrd_x (pg, s8, 8);
  s8 = svasrd_x (pg, s8, 9); /* { dg-error {passing 9 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 8\]} } */
  s8 = svasrd_x (pg, s8, (uint64_t (1) << 62) + 1); /* { dg-error {passing [^ ]* to argument 3 of 'svasrd_x', which expects a value in the range \[1, 8\]} } */
  s16 = svasrd_x (pg, s16, 0); /* { dg-error {passing 0 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 16\]} } */
  s16 = svasrd_x (pg, s16, 1);
  s16 = svasrd_x (pg, s16, 16);
  s16 = svasrd_x (pg, s16, 17); /* { dg-error {passing 17 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 16\]} } */
  s32 = svasrd_x (pg, s32, 0); /* { dg-error {passing 0 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 32\]} } */
  s32 = svasrd_x (pg, s32, 1);
  s32 = svasrd_x (pg, s32, 32);
  s32 = svasrd_x (pg, s32, 33); /* { dg-error {passing 33 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 32\]} } */
  s64 = svasrd_x (pg, s64, 0); /* { dg-error {passing 0 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 64\]} } */
  s64 = svasrd_x (pg, s64, 1);
  s64 = svasrd_x (pg, s64, 64);
  s64 = svasrd_x (pg, s64, 65); /* { dg-error {passing 65 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 64\]} } */
}
