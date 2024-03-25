/* { dg-do compile } */
/* { dg-options "-std=c99 -Wpointer-sign" } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

struct s { int i; };

void
f1 (svbool_t pg, short *s16_ptr, unsigned short *u16_ptr,
    svint8_t s8, svint16_t s16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, struct s s)
{
  svldnt1sh_gather_index (pg, s16_ptr, s64); /* { dg-error {implicit declaration of function 'svldnt1sh_gather_index'; did you mean 'svldnt1_gather_index'} } */
  svldnt1sh_gather_index_u64 (pg, s16_ptr); /* { dg-error {too few arguments to function 'svldnt1sh_gather_index_u64'} } */
  svldnt1sh_gather_index_u64 (pg, s16_ptr, s64, 0); /* { dg-error {too many arguments to function 'svldnt1sh_gather_index_u64'} } */
  svldnt1sh_gather_index_u64 (pg, u16_ptr, s64); /* { dg-warning {pointer targets in passing argument 2 of 'svldnt1sh_gather_s64index_u64' differ in signedness} } */
  svldnt1sh_gather_index_u64 (pg, s16_ptr, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svldnt1sh_gather_index_u64', which expects a vector of 64-bit integers} } */
  svldnt1sh_gather_index_u64 (pg, s16_ptr, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svldnt1sh_gather_index_u64', which expects a vector of 64-bit integers} } */
  svldnt1sh_gather_index_u64 (pg, s16_ptr, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svldnt1sh_gather_index_u64', which expects a vector of 64-bit integers} } */
  svldnt1sh_gather_index_u64 (pg, s16_ptr, s64);
  svldnt1sh_gather_index_u64 (pg, s16_ptr, u64);
  svldnt1sh_gather_index_u64 (pg, s16_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1sh_gather_index_u64', which expects a vector of 64-bit integers} } */
  svldnt1sh_gather_index_u64 (pg, s16_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svldnt1sh_gather_index_u64', which expects a vector of 64-bit integers} } */
  svldnt1sh_gather_index_u64 (pg, s16_ptr, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svldnt1sh_gather_index_u64', which expects a vector of 64-bit integers} } */
  svldnt1sh_gather_index_u64 (pg, s16_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1sh_gather_index_u64', which expects a vector of 64-bit integers} } */

  svldnt1sh_gather_index_u64 (pg, 0, s64);
  svldnt1sh_gather_index_u64 (pg, s, s64); /* { dg-error {'struct s' to argument 2 of 'svldnt1sh_gather_index_u64', which expects a vector or pointer base address} } */

  svldnt1sh_gather_index_u64 (pg, pg, 0); /* { dg-error {passing 'svbool_t' to argument 2 of 'svldnt1sh_gather_index_u64', which expects 'svuint64_t'} } */
  svldnt1sh_gather_index_u64 (pg, s64, 0); /* { dg-error {passing 'svint64_t' to argument 2 of 'svldnt1sh_gather_index_u64', which expects 'svuint64_t'} } */
  svldnt1sh_gather_index_u64 (pg, u64, 0);
  svldnt1sh_gather_index_u64 (pg, u32, 0); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svldnt1sh_gather_index_u64', which expects 'svuint64_t'} } */

  svldnt1sh_gather_index_u32 (pg, u16_ptr, s32); /* { dg-error {'svldnt1sh_gather_index_u32' requires a vector base and a scalar index} } */
  svldnt1sh_gather_index_u32 (pg, s16_ptr, pg); /* { dg-error {'svldnt1sh_gather_index_u32' requires a vector base and a scalar index} } */
  svldnt1sh_gather_index_u32 (pg, s16_ptr, s8); /* { dg-error {'svldnt1sh_gather_index_u32' requires a vector base and a scalar index} } */
  svldnt1sh_gather_index_u32 (pg, s16_ptr, s16); /* { dg-error {'svldnt1sh_gather_index_u32' requires a vector base and a scalar index} } */
  svldnt1sh_gather_index_u32 (pg, s16_ptr, 0); /* { dg-error {'svldnt1sh_gather_index_u32' requires a vector base and a scalar index} } */
}
