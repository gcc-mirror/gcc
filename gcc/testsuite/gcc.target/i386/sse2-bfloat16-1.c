/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse2" } */

__bf16/* { dg-error "unknown type name '__bf16'" } */
foo (__bf16 x) /* { dg-error "unknown type name '__bf16'" } */
{
  return x;
}
