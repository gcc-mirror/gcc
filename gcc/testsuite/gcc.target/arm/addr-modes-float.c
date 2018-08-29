/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

#include "addr-modes.h"

#define ATTR __attribute__((__pcs__("aapcs-vfp")))

POST_STORE(float, ATTR)
/* { dg-final { scan-assembler "vstmia.32" } } */
POST_STORE(double, ATTR)
/* { dg-final { scan-assembler "vstmia.64" } } */

POST_LOAD(float, ATTR)
/* { dg-final { scan-assembler "vldmia.32" } } */
POST_LOAD(double, ATTR)
/* { dg-final { scan-assembler "vldmia.64" } } */

POST_STORE_VEC (int8_t, int8x8_t, vst1_s8, ATTR)
/* { dg-final { scan-assembler "vst1.8\t\{.*\}, \\\[r\[0-9\]+\\\]!" } } */
POST_STORE_VEC (int8_t, int8x16_t, vst1q_s8, ATTR)
/* { dg-final { scan-assembler "vst1.8\t\{.*\[-,\]d.*\}, \\\[r\[0-9\]+\\\]!" } } */

POST_STORE_VEC (int8_t, int8x8x2_t, vst2_s8, ATTR)
/* { dg-final { scan-assembler "vst2.8\t\{.*\}, \\\[r\[0-9\]+\\\]!" } } */
POST_STORE_VEC (int8_t, int8x16x2_t, vst2q_s8, ATTR)
/* { dg-final { scan-assembler "vst2.8\t\{.*-d.*\}, \\\[r\[0-9\]+\\\]!" } } */

POST_STORE_VEC (int8_t, int8x8x3_t, vst3_s8, ATTR)
/* { dg-final { scan-assembler "vst3.8\t\{.*\}, \\\[r\[0-9\]+\\\]!" } } */
POST_STORE_VEC (int8_t, int8x16x3_t, vst3q_s8, ATTR)
/* { dg-final { scan-assembler "vst3.8\t\{d\[02468\], d\[02468\], d\[02468\]\}, \\\[r\[0-9\]+\\\]!" } } */
/* { dg-final { scan-assembler "vst3.8\t\{d\[13579\], d\[13579\], d\[13579\]\}, \\\[r\[0-9\]+\\\]!" { xfail *-*-* } } } */

POST_STORE_VEC (int8_t, int8x8x4_t, vst4_s8, ATTR)
/* { dg-final { scan-assembler "vst4.8\t\{.*\}, \\\[r\[0-9\]+\\\]!" } } */
POST_STORE_VEC (int8_t, int8x16x4_t, vst4q_s8, ATTR)
/* { dg-final { scan-assembler "vst4.8\t\{d\[02468\], d\[02468\], d\[02468\], d\[02468\]\}, \\\[r\[0-9\]+\\\]!" } } */
/* { dg-final { scan-assembler "vst4.8\t\{d\[13579\], d\[13579\], d\[13579\], d\[13579\]\}, \\\[r\[0-9\]+\\\]!" { xfail *-*-* } } } */

/* { dg-final { scan-assembler-not "add" { xfail *-*-* } } } */
