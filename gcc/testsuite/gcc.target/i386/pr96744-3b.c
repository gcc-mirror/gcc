/* { dg-do compile } */
/* { dg-options "-msse2" } */

typedef int int32x4_t __attribute__ ((__vector_size__ ((16))));
extern int32x4_t c;

#pragma GCC push_options
#pragma GCC target("general-regs-only")

void
test (int32x4_t a, int32x4_t b) /* { dg-warning "SSE vector argument without SSE enabled" } */
{
  c = a + b;
}

#pragma GCC pop_options
