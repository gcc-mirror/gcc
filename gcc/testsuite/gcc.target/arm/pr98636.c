/* { dg-do compile } */
/* { dg-require-effective-target arm_softfp_ok } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-mfloat-abi=softfp" } */
/* { dg-add-options arm_fp16_alternative } */

#pragma GCC push_options
# pragma GCC target ("arch=armv8.2-a+fp16") /* { dg-error "selected fp16 options are incompatible" } */
#pragma GCC pop_options
