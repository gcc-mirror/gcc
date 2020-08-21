/* { dg-do compile { target ia32 } } */
/* { dg-options "-msse2" } */

typedef int int32x4_t __attribute__ ((__vector_size__ ((16))));

__attribute__((__target__("general-regs-only")))
int32x4_t
test (int32x4_t a, int32x4_t b) /* { dg-warning "SSE vector argument without SSE enabled" } */
{ /* { dg-warning "SSE vector return without SSE enabled" } */
  return a + b;
}
