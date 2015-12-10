/* Check that calling a neon builtin from a function compiled with vfp fails.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 -mfloat-abi=softfp" } */

#include <arm_neon.h>

__attribute__ ((target ("fpu=vfp")))
void 
foo (uint8x16_t *p)
{
  *p = vmovq_n_u8 (3); /* { dg-message "called from here" } */
}

/* { dg-error "inlining failed in call to always_inline" "" { target *-*-* } 0 }
 */

