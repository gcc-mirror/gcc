/* { dg-do compile } */
/* { dg-options "-std=c99 -Wpointer-sign" } */

#include <arm_sve.h>

struct s { int i; };

void
f1 (svbool_t pg, short *s16_ptr, unsigned short *u16_ptr,
    svint8_t s8, svint16_t s16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, struct s s)
{
  svld1sh_gather_index (pg, s16_ptr, s32); /* { dg-error {implicit declaration of function 'svld1sh_gather_index'; did you mean 'svld1_gather_index'} } */
  svld1sh_gather_index_u32 (pg, s16_ptr); /* { dg-error {too few arguments to function 'svld1sh_gather_index_u32'} } */
  svld1sh_gather_index_u32 (pg, s16_ptr, s32, 0); /* { dg-error {too many arguments to function 'svld1sh_gather_index_u32'} } */
  svld1sh_gather_index_u32 (pg, u16_ptr, s32); /* { dg-warning {pointer targets in passing argument 2 of 'svld1sh_gather_s32index_u32' differ in signedness} } */
  svld1sh_gather_index_u32 (pg, s16_ptr, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svld1sh_gather_index_u32', which expects a vector of 32-bit integers} } */
  svld1sh_gather_index_u32 (pg, s16_ptr, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svld1sh_gather_index_u32', which expects a vector of 32-bit integers} } */
  svld1sh_gather_index_u32 (pg, s16_ptr, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svld1sh_gather_index_u32', which expects a vector of 32-bit integers} } */
  svld1sh_gather_index_u32 (pg, s16_ptr, s32);
  svld1sh_gather_index_u32 (pg, s16_ptr, u32);
  svld1sh_gather_index_u32 (pg, s16_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svld1sh_gather_index_u32', which expects a vector of 32-bit integers} } */
  svld1sh_gather_index_u32 (pg, s16_ptr, s64); /* { dg-error {passing 'svint64_t' to argument 3 of 'svld1sh_gather_index_u32', which expects a vector of 32-bit integers} } */
  svld1sh_gather_index_u32 (pg, s16_ptr, u64); /* { dg-error {passing 'svuint64_t' to argument 3 of 'svld1sh_gather_index_u32', which expects a vector of 32-bit integers} } */
  svld1sh_gather_index_u32 (pg, s16_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svld1sh_gather_index_u32', which expects a vector of 32-bit integers} } */

  svld1sh_gather_index_u32 (pg, 0, s32);
  svld1sh_gather_index_u32 (pg, s, s32); /* { dg-error {'struct s' to argument 2 of 'svld1sh_gather_index_u32', which expects a vector or pointer base address} } */

  svld1sh_gather_index_u32 (pg, pg, 0); /* { dg-error {passing 'svbool_t' to argument 2 of 'svld1sh_gather_index_u32', which expects 'svuint32_t'} } */
  svld1sh_gather_index_u32 (pg, s32, 0); /* { dg-error {passing 'svint32_t' to argument 2 of 'svld1sh_gather_index_u32', which expects 'svuint32_t'} } */
  svld1sh_gather_index_u32 (pg, u32, 0);
  svld1sh_gather_index_u32 (pg, u64, 0); /* { dg-error {passing 'svuint64_t' to argument 2 of 'svld1sh_gather_index_u32', which expects 'svuint32_t'} } */
}
