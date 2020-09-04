/* { dg-do compile } */
/* { dg-options "-mpure-code" } */

int f3 (void) { return 0x11000000; }
int f3_2 (void) { return 0x12345678; }
int f3_3 (void) { return -1; }
int f3_4 (void) { return 511; }

/* For cortex-m0 (thumb-1/v6m), we generate 1 lsls in f3 3 lsls in f3_2 and 1 in f3_4; 1 rsbs in f3_3.  */
/* { dg-final { scan-assembler-times "lsls" 5 { target { { ! arm_thumb1_movt_ok } && { ! arm_thumb2_ok } } } } } */
/* { dg-final { scan-assembler-times "rsbs" 1 { target { { ! arm_thumb1_movt_ok } && { ! arm_thumb2_ok } } } } } */

/* For cortex-m23 (thumb-1/v8m.base), we generate 1 lsls in f3, and none in f3_2 nor f3_4; 1 rsbs in f3_3.  */
/* { dg-final { scan-assembler-times "lsls" 1 { target { arm_thumb1_movt_ok && { ! arm_thumb2_ok } } } } } */
/* { dg-final { scan-assembler-times "rsbs" 1 { target { arm_thumb1_movt_ok && { ! arm_thumb2_ok } } } } } */

/* For cortex-m3 (thumb-2/v7m), we generate no lsls and no rsbs.  */
/* { dg-final { scan-assembler-times "lsls" 0 { target { arm_thumb2_ok } } } } */
/* { dg-final { scan-assembler-times "rsbs" 0 { target { arm_thumb2_ok } } } } */

