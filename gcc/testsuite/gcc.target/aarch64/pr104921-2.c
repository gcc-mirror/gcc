/* { dg-do assemble } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-O2 -std=gnu99" }  */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */

#include "pr104921.x"
