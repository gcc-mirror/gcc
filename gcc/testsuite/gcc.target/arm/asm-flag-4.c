/* Test that we do not ice in thumb1 mode */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v4t_thumb_ok } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=softfp" } } */
/* { dg-options "-march=armv4t -mfloat-abi=softfp" } */

void __attribute__((target("arm"))) f(char *out)
{
  asm("" : "=@ccne"(out[0]));
}

void __attribute__((target("thumb"))) g(char *out)
{
  asm("" : "=@ccne"(out[0]));  /* { dg-message "asm flags not supported" } */
}
