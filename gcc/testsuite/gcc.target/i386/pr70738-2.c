/* { dg-do compile { target ia32 } } */
/* { dg-options "-mmmx -mgeneral-regs-only" } */

typedef int int32x2_t __attribute__ ((__vector_size__ ((8))));

int32x2_t
test (int32x2_t a, int32x2_t b) /* { dg-warning "MMX vector argument without MMX enabled" } */
{ /* { dg-warning "MMX vector return without MMX enabled" } */
  return a + b;
}
