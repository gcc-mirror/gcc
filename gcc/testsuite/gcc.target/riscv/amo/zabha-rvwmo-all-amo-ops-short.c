/* Test __atomic routines for existence on 2 byte values with each valid memory model.  */
/* { dg-do compile } */
/* { dg-options "-Wno-address-of-packed-member" } */
/* { dg-add-options riscv_zabha } */
/* { dg-remove-options riscv_ztso } */
/* { dg-final { scan-assembler "\tamoadd.h" } } */
/* { dg-final { scan-assembler "\tamoadd.h.aq" } } */
/* { dg-final { scan-assembler "\tamoadd.h.rl" } } */
/* { dg-final { scan-assembler "\tamoadd.h.aqrl" } } */
/* { dg-final { scan-assembler "\tamoand.h" } } */
/* { dg-final { scan-assembler "\tamoand.h.aq" } } */
/* { dg-final { scan-assembler "\tamoand.h.rl" } } */
/* { dg-final { scan-assembler "\tamoand.h.aqrl" } } */
/* { dg-final { scan-assembler "\tamoxor.h" } } */
/* { dg-final { scan-assembler "\tamoxor.h.aq" } } */
/* { dg-final { scan-assembler "\tamoxor.h.rl" } } */
/* { dg-final { scan-assembler "\tamoxor.h.aqrl" } } */
/* { dg-final { scan-assembler "\tamoor.h" } } */
/* { dg-final { scan-assembler "\tamoor.h.aq" } } */
/* { dg-final { scan-assembler "\tamoor.h.rl" } } */
/* { dg-final { scan-assembler "\tamoor.h.aqrl" } } */

#include "inline-atomics-4.c"
