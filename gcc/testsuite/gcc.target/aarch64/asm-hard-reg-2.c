/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+sve" } */

/* Test register pairs.  */

#include <arm_sve.h>

void
test (void)
{
  svuint32x2_t x, y;
  svuint32x4_t z;

  __asm__ __volatile__ ("" : "={z4}" (x), "={z6}" (y));
  __asm__ __volatile__ ("" : "={z5}" (x), "={z6}" (y));  /* { dg-error "multiple outputs to hard register: v6" } */
  __asm__ __volatile__ ("" : "={z4}" (z), "={z6}" (y));  /* { dg-error "multiple outputs to hard register: v6" } */
}
