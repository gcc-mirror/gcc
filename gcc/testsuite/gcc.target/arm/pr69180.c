/* PR target/69180
   Check that __ARM_NEON_FP redefinition warns for user setting and not for
   #pragma GCC target.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-mfloat-abi=softfp -mfpu=neon" } */

#pragma GCC target ("fpu=neon-fp-armv8")

#define __ARM_NEON_FP 0
#define __ARM_FP 0
#define __ARM_FEATURE_LDREX 0

/* { dg-warning ".__ARM_NEON_FP. redefined" "" { target *-*-* } 10 }  */
/* { dg-warning ".__ARM_FP. redefined" "" { target *-*-* } 11 } */
/* { dg-warning ".__ARM_FEATURE_LDREX. redefined" "" { target *-*-* } 12 } */
