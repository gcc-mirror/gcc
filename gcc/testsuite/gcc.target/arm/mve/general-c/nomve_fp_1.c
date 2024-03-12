/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-require-effective-target arm_fp_ok } */
/* Do not use dg-add-options arm_v8_1m_mve, because this might expand to "",
   which could imply mve+fp depending on the user settings. We want to make
   sure the '+fp' extension is not enabled.  */
/* { dg-options "-mfpu=auto -march=armv8.1-m.main+mve" } */
/* { dg-add-options arm_fp } */

#include <arm_mve.h>

void
foo (uint8x16_t v)
{
  vreinterpretq_f16 (v); /* { dg-error {ACLE function '__arm_vreinterpretq_f16_u8' requires ISA extension 'mve.fp'} } */
  /* { dg-message {note: you can enable mve.fp by using the command-line option '-march', or by using the 'target' attribute or pragma} "" {target *-*-*} .-1 } */
}
