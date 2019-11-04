/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

void
f1 (svbool_t pg, uint32_t *u32_ptr, svuint8_t u8, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64)
{
  svadrb_offset (u32); /* { dg-error {too few arguments to function 'svadrb_offset'} } */
  svadrb_offset (u32, u32, u32); /* { dg-error {too many arguments to function 'svadrb_offset'} } */
  svadrb_offset (u32_ptr, s32); /* { dg-error {passing '[^']*\*'[^\n]* to argument 1 of 'svadrb_offset', which expects an SVE vector type} } */
  svadrb_offset (0, s32); /* { dg-error {passing 'int' to argument 1 of 'svadrb_offset', which expects an SVE vector type} } */
  svadrb_offset (u16, u16); /* { dg-error {passing 'svuint16_t' to argument 1 of 'svadrb_offset', which expects 'svuint32_t' or 'svuint64_t'} } */
  svadrb_offset (s32, s32); /* { dg-error {passing 'svint32_t' to argument 1 of 'svadrb_offset', which expects 'svuint32_t' or 'svuint64_t'} } */
  svadrb_offset (f32, s32); /* { dg-error {passing 'svfloat32_t' to argument 1 of 'svadrb_offset', which expects 'svuint32_t' or 'svuint64_t'} } */
  svadrb_offset (pg, s32); /* { dg-error {passing 'svbool_t' to argument 1 of 'svadrb_offset', which expects 'svuint32_t' or 'svuint64_t'} } */

  svadrb_offset (u32, 0); /* { dg-error {passing 'int' to argument 2 of 'svadrb_offset', which expects an SVE vector type} } */
  svadrb_offset (u32, u8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svadrb_offset', which expects a vector of 32-bit or 64-bit integers} } */
  svadrb_offset (u32, u16); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svadrb_offset', which expects a vector of 32-bit or 64-bit integers} } */
  svadrb_offset (u32, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svadrb_offset', which expects a vector of integers} } */

  svadrb_offset (u32, s32);
  svadrb_offset (u32, u32);
  svadrb_offset (u32, f32); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svadrb_offset', which expects a vector of integers} } */
  svadrb_offset (u32, s64); /* { dg-error {cannot combine a base of type 'svuint32_t' with an offset of type 'svint64_t'} } */
  svadrb_offset (u32, u64); /* { dg-error {cannot combine a base of type 'svuint32_t' with an offset of type 'svuint64_t'} } */
  svadrb_offset (u32, f64); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svadrb_offset', which expects a vector of integers} } */

  svadrb_offset (u64, s32); /* { dg-error {cannot combine a base of type 'svuint64_t' with an offset of type 'svint32_t'} } */
  svadrb_offset (u64, u32); /* { dg-error {cannot combine a base of type 'svuint64_t' with an offset of type 'svuint32_t'} } */
  svadrb_offset (u64, f32); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svadrb_offset', which expects a vector of integers} } */
  svadrb_offset (u64, s64);
  svadrb_offset (u64, u64);
  svadrb_offset (u64, f64); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svadrb_offset', which expects a vector of integers} } */
}
