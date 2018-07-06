/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm_hf_eabi } */
/* { dg-add-options arm_neon } */
/* { dg-additional-options "-mfp16-format=ieee -mfloat-abi=hard" } */

typedef __fp16 v4f16
  __attribute__ ((vector_size (8)));

v4f16 fn1 (v4f16 p)
{
  return p;
}
