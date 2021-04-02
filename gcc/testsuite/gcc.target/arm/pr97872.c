/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O3" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint8x8_t f1(int8x8_t a, int8x8_t b) {
  return a < b;
}

/* { dg-final { scan-assembler-not "vbsl" } } */
