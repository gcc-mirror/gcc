/* PR target/69180
   Check that __ARM_NEON_FP redefinition warns for user setting and not for
   #pragma GCC target.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options " " } */ /* Necessary to  prevent the harness from adding -ansi -pedantic-errors to the command line.  */
/* { dg-add-options arm_neon } */
#pragma GCC target ("fpu=neon-fp-armv8")

#define __ARM_NEON_FP 0
/* { dg-warning "-:.__ARM_NEON_FP. redefined" "" { target *-*-* } .-1 }  */

#define __ARM_FP 0
/* { dg-warning "-:.__ARM_FP. redefined" "" { target *-*-* } .-1 } */

#define __ARM_FEATURE_LDREX 0
/* { dg-warning "-:.__ARM_FEATURE_LDREX. redefined" "" { target *-*-* } .-1 } */
