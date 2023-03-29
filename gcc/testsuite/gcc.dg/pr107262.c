/* PR middle-end/107262 */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */
/* { dg-add-options bfloat16 } */
/* { dg-require-effective-target bfloat16_runtime } */

__bf16
foo (__bf16 a)
{
  __bf16 b = 0;
  b /= a;
  return b;
}
