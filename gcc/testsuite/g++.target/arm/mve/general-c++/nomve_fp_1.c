/* { dg-do compile } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* Do not use dg-add-options arm_v8_1m_mve, because this might expand to "",
   which could imply mve+fp depending on the user settings. We want to make
   sure the '+fp' extension is not enabled.  */
/* { dg-options "-mfpu=auto -mcpu=unset -march=armv8.1-m.main+mve" } */
/* { dg-add-options arm_fp } */

#include <arm_mve.h>

void
f1 (uint8x16_t v)
{
  vreinterpretq_f16 (v); /* { dg-error {ACLE function 'void vreinterpretq_f16\(uint8x16_t\)' requires ISA extension 'mve.fp'} } */
  /* { dg-message {note: you can enable mve.fp by using the command-line option '-march', or by using the 'target' attribute or pragma} "" {target *-*-*} .-1 } */
}
