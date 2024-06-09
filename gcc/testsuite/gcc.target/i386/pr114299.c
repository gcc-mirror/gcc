/* { dg-do compile { target lp64 } } */
/* { dg-options "-mgeneral-regs-only" } */

typedef __attribute__((__vector_size__(8))) __bf16 V;
typedef __attribute__((__vector_size__(16))) __bf16 W;

V v;
_Atomic V a;

W
foo(void) /* { dg-error "SSE" } */
{
  return __builtin_shufflevector(v, a, 1, 2, 5, 0, 1, 6, 6, 4); /* { dg-error "invalid" } */
}
