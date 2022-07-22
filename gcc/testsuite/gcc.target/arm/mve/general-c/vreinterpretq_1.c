/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#include <arm_mve.h>

void
f1 (int8x16_t s8, uint16x8_t u16, float32x4_t f32)
{
  __arm_vreinterpretq_s8 (); /* { dg-error {too few arguments to function '__arm_vreinterpretq_s8'} } */
  __arm_vreinterpretq_s8 (s8, s8); /* { dg-error {too many arguments to function '__arm_vreinterpretq_s8'} } */
  __arm_vreinterpretq_s8 (0); /* { dg-error {passing 'int' to argument 1 of '__arm_vreinterpretq_s8', which expects an MVE vector type} } */
  __arm_vreinterpretq_s8 (s8); /* { dg-error {'__arm_vreinterpretq_s8' has no form that takes 'int8x16_t' arguments} } */
  __arm_vreinterpretq_s8 (u16);
  __arm_vreinterpretq_u16 (); /* { dg-error {too few arguments to function '__arm_vreinterpretq_u16'} } */
  __arm_vreinterpretq_u16 (u16, u16); /* { dg-error {too many arguments to function '__arm_vreinterpretq_u16'} } */
  __arm_vreinterpretq_u16 (0); /* { dg-error {passing 'int' to argument 1 of '__arm_vreinterpretq_u16', which expects an MVE vector type} } */
  __arm_vreinterpretq_u16 (u16); /* { dg-error {'__arm_vreinterpretq_u16' has no form that takes 'uint16x8_t' arguments} } */
  __arm_vreinterpretq_u16 (f32);
  __arm_vreinterpretq_f32 (); /* { dg-error {too few arguments to function '__arm_vreinterpretq_f32'} } */
  __arm_vreinterpretq_f32 (f32, f32); /* { dg-error {too many arguments to function '__arm_vreinterpretq_f32'} } */
  __arm_vreinterpretq_f32 (0); /* { dg-error {passing 'int' to argument 1 of '__arm_vreinterpretq_f32', which expects an MVE vector type} } */
  __arm_vreinterpretq_f32 (f32); /* { dg-error {'__arm_vreinterpretq_f32' has no form that takes 'float32x4_t' arguments} } */
  __arm_vreinterpretq_f32 (s8);
}
