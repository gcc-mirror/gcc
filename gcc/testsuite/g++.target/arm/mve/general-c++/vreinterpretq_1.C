/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#include <arm_mve.h>

void
f1 (int8x16_t s8, uint16x8_t u16, float32x4_t f32)
{
  __arm_vreinterpretq_s8 (); /* { dg-error {no matching function for call to '__arm_vreinterpretq_s8\(\)'} } */
  __arm_vreinterpretq_s8 (s8, s8); /* { dg-error {no matching function for call to '__arm_vreinterpretq_s8\(int8x16_t\&, int8x16_t\&\)'} } */
  __arm_vreinterpretq_s8 (0); /* { dg-error {no matching function for call to '__arm_vreinterpretq_s8\(int\)'} } */
  __arm_vreinterpretq_s8 (s8); /* { dg-error {no matching function for call to '__arm_vreinterpretq_s8\(int8x16_t\&\)'} } */
  __arm_vreinterpretq_s8 (u16);
  __arm_vreinterpretq_u16 (); /* { dg-error {no matching function for call to '__arm_vreinterpretq_u16\(\)'} } */
  __arm_vreinterpretq_u16 (u16, u16); /* { dg-error {no matching function for call to '__arm_vreinterpretq_u16\(uint16x8_t\&, uint16x8_t\&\)'} } */
  __arm_vreinterpretq_u16 (0); /* { dg-error {no matching function for call to '__arm_vreinterpretq_u16\(int\)'} } */
  __arm_vreinterpretq_u16 (u16); /* { dg-error {no matching function for call to '__arm_vreinterpretq_u16\(uint16x8_t\&\)'} } */
  __arm_vreinterpretq_u16 (f32);
  __arm_vreinterpretq_f32 (); /* { dg-error {no matching function for call to '__arm_vreinterpretq_f32\(\)'} } */
  __arm_vreinterpretq_f32 (f32, f32); /* { dg-error {no matching function for call to '__arm_vreinterpretq_f32\(float32x4_t\&, float32x4_t\&\)'} } */
  __arm_vreinterpretq_f32 (0); /* { dg-error {no matching function for call to '__arm_vreinterpretq_f32\(int\)'} } */
  __arm_vreinterpretq_f32 (f32); /* { dg-error {no matching function for call to '__arm_vreinterpretq_f32\(float32x4_t\&\)'} } */
  __arm_vreinterpretq_f32 (s8);
}
