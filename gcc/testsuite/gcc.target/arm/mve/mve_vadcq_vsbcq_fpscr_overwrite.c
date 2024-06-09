/* { dg-do run } */
/* { dg-require-effective-target arm_mve_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_v8_1m_mve } */

#include <arm_mve.h>

volatile int32x4_t c1;
volatile uint32x4_t c2;
int carry;

int
main ()
{
  int32x4_t a1 = vcreateq_s32 (0, 0);
  int32x4_t b1 = vcreateq_s32 (0, 0);
  int32x4_t inactive1 = vcreateq_s32 (0, 0);

  uint32x4_t a2 = vcreateq_u32 (0, 0);
  uint32x4_t b2 = vcreateq_u32 (0, 0);
  uint32x4_t inactive2 = vcreateq_u32 (0, 0);

  mve_pred16_t p = 0xFFFF;
  carry = 0xFFFFFFFF;

  __builtin_arm_set_fpscr_nzcvqc (0);
  c1 = vadcq (a1, b1, &carry);
  if (__builtin_arm_get_fpscr_nzcvqc () & !0x20000000)
    __builtin_abort ();
  carry = 0xFFFFFFFF;
  __builtin_arm_set_fpscr_nzcvqc (0);
  c2 = vadcq (a2, b2, &carry);
  if (__builtin_arm_get_fpscr_nzcvqc () & !0x20000000)
    __builtin_abort ();
  carry = 0xFFFFFFFF;
  __builtin_arm_set_fpscr_nzcvqc (0);
  c1 = vsbcq (a1, b1, &carry);
  if (__builtin_arm_get_fpscr_nzcvqc () & !0x20000000)
    __builtin_abort ();
  carry = 0xFFFFFFFF;
  __builtin_arm_set_fpscr_nzcvqc (0);
  c2 = vsbcq (a2, b2, &carry);
  if (__builtin_arm_get_fpscr_nzcvqc () & !0x20000000)
    __builtin_abort ();
  carry = 0xFFFFFFFF;
  __builtin_arm_set_fpscr_nzcvqc (0);
  c1 = vadcq_m (inactive1, a1, b1, &carry, p);
  if (__builtin_arm_get_fpscr_nzcvqc () & !0x20000000)
    __builtin_abort ();
  carry = 0xFFFFFFFF;
  __builtin_arm_set_fpscr_nzcvqc (0);
  c2 = vadcq_m (inactive2, a2, b2, &carry, p);
  if (__builtin_arm_get_fpscr_nzcvqc () & !0x20000000)
    __builtin_abort ();
  carry = 0xFFFFFFFF;
  __builtin_arm_set_fpscr_nzcvqc (0);
  c1 = vsbcq_m (inactive1, a1, b1, &carry, p);
  if (__builtin_arm_get_fpscr_nzcvqc () & !0x20000000)
    __builtin_abort ();
  carry = 0xFFFFFFFF;
  __builtin_arm_set_fpscr_nzcvqc (0);
  c2 = vsbcq_m (inactive2, a2, b2, &carry, p);
  if (__builtin_arm_get_fpscr_nzcvqc () & !0x20000000)
    __builtin_abort ();

  return 0;
}
