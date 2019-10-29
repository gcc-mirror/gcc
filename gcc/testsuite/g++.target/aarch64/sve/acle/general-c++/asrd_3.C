/* { dg-do compile } */
/* { dg-additional-options "-std=c++11 -Wall -Wextra" } */

#include <arm_sve.h>

constexpr uint64_t const_add (uint64_t a, uint64_t b) { return a + b; }
uint64_t add (uint64_t a, uint64_t b) { return a + b; }

template<uint64_t N, typename T>
T shift (svbool_t pg, T v) { return svasrd_x (pg, v, N); }
/* { dg-error {no matching function for call to 'svasrd_x\(svbool_t&,} "" { target *-*-* } .-1 } */
/* { dg-error {passing 0 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 8\]} "" { target *-*-* } .-2 } */
/* { dg-error {passing 9 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 8\]} "" { target *-*-* } .-3 } */
/* { dg-error {passing 0 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 16\]} "" { target *-*-* } .-4 } */
/* { dg-error {passing 17 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 16\]} "" { target *-*-* } .-5 } */
/* { dg-error {passing 0 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 32\]} "" { target *-*-* } .-6 } */
/* { dg-error {passing 33 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 32\]} "" { target *-*-* } .-7 } */
/* { dg-error {passing 0 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 64\]} "" { target *-*-* } .-8 } */
/* { dg-error {passing 65 to argument 3 of 'svasrd_x', which expects a value in the range \[1, 64\]} "" { target *-*-* } .-9 } */

template<typename T>
T shift1 (svbool_t pg, T v, uint64_t n) { return svasrd_x (pg, v, n); }

template<typename T>
T shift2 (svbool_t pg, T v, uint64_t n) { return svasrd_x (pg, v, n); }
/* { dg-error {argument 3 of 'svasrd_x' must be an integer constant expression} "" { target *-*-* } .-1 } */

void
f1 (svbool_t pg, svuint8_t u8, svint8_t s8, svint16_t s16,
    svint32_t s32, svint64_t s64)
{
  u8 = shift <1> (pg, u8);
  s8 = shift <0> (pg, s8);
  s8 = shift <1> (pg, s8);
  s8 = shift <8> (pg, s8);
  s8 = shift <9> (pg, s8);
  s16 = shift <0> (pg, s16);
  s16 = shift <1> (pg, s16);
  s16 = shift <16> (pg, s16);
  s16 = shift <17> (pg, s16);
  s32 = shift <0> (pg, s32);
  s32 = shift <1> (pg, s32);
  s32 = shift <32> (pg, s32);
  s32 = shift <33> (pg, s32);
  s64 = shift <0> (pg, s64);
  s64 = shift <1> (pg, s64);
  s64 = shift <64> (pg, s64);
  s64 = shift <65> (pg, s64);

  s8 = shift2 (pg, s8, 1);
}
