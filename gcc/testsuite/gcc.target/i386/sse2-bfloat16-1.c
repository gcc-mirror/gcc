/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse2" } */

__bf16
foo (__bf16 x)  /* { dg-error "SSE register return with SSE2 disabled" } */
{  /* { dg-error "SSE register return with SSE2 disabled" "" { target ia32 } } */
  return x;  /* { dg-error "SSE register return with SSE2 disabled" "" { target ia32} } */
}
