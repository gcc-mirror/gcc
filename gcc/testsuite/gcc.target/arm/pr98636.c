/* { dg-do compile } */
/* { dg-require-effective-target arm_softfp_ok } */
/* { dg-options "-mfp16-format=alternative -mfloat-abi=softfp" } */

#pragma GCC push_options
# pragma GCC target ("arch=armv8.2-a+fp16") /* { dg-error "selected fp16 options are incompatible" } */
#pragma GCC pop_options
