/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-pedantic -std=gnu99" } */
/* { dg-add-options arm_fp16_alternative } */

#include <math.h>

/* Infinities are not representable in the alternative format;
   we should get a diagnostic, and the value set to the largest
   representable value.  */
/* 0x7fff = 32767 */
__fp16 xx = INFINITY; /* { dg-warning "overflow" } */

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t32767" } } */
