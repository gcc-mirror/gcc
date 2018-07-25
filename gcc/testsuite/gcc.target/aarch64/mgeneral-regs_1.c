/* { dg-options "-mgeneral-regs-only" } */

typedef int int32x2_t __attribute__ ((__vector_size__ ((8))));

/* { dg-error "'-mgeneral-regs-only' is incompatible with the use of vector types" "" {target "aarch64*-*-*"} .+1 } */
int32x2_t test (int32x2_t a, int32x2_t b)
{
  return a + b;
}
