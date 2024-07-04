/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

struct s { signed char x; };

svuint32_t
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16,
    svfloat16_t f16, svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64)
{
  svst1_scatter (pg, u32); /* { dg-error {too few arguments to function 'svst1_scatter'} } */
  svst1_scatter (pg, u32, u32, 0); /* { dg-error {too many arguments to function 'svst1_scatter'} } */
  svst1_scatter (0, u32, u32); /* { dg-error {passing 'int' to argument 1 of 'svst1_scatter', which expects 'svbool_t'} } */
  svst1_scatter (pg, 0, u32); /* { dg-error {passing 'int' to argument 2 of 'svst1_scatter', which expects an SVE type rather than a scalar} } */
  svst1_scatter (pg, u32, 0); /* { dg-error {passing 'int' to argument 3 of 'svst1_scatter', which expects an SVE type rather than a scalar} } */

  svst1_scatter (pg, u32, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svst1_scatter', which expects a vector of 32-bit or 64-bit elements} } */

  svst1_scatter (pg, u32, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svst1_scatter', which expects a vector of 32-bit or 64-bit elements} } */
  svst1_scatter (pg, u32, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svst1_scatter', which expects a vector of 32-bit or 64-bit elements} } */

  svst1_scatter (pg, u32, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svst1_scatter', which expects a vector of 32-bit or 64-bit elements} } */
  svst1_scatter (pg, u32, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svst1_scatter', which expects a vector of 32-bit or 64-bit elements} } */
  svst1_scatter (pg, u32, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svst1_scatter', which expects a vector of 32-bit or 64-bit elements} } */

  svst1_scatter (pg, u32, s32);
  svst1_scatter (pg, s32, s32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svst1_scatter', which expects 'svuint32_t'} } */

  svst1_scatter (pg, u32, u32);
  svst1_scatter (pg, s32, u32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svst1_scatter', which expects 'svuint32_t'} } */

  svst1_scatter (pg, u32, f32);
  svst1_scatter (pg, s32, f32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svst1_scatter', which expects 'svuint32_t'} } */

  svst1_scatter (pg, u64, s64);
  svst1_scatter (pg, s64, s64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1_scatter', which expects 'svuint64_t'} } */

  svst1_scatter (pg, u64, u64);
  svst1_scatter (pg, s64, u64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1_scatter', which expects 'svuint64_t'} } */

  svst1_scatter (pg, u64, f64);
  svst1_scatter (pg, s64, f64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1_scatter', which expects 'svuint64_t'} } */
}
