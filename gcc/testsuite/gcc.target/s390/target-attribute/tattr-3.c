/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-O3 -march=z13 -mzarch -mzvector" } */

#include <vecintrin.h>

__attribute__ ((target("arch=z13")))
void vx1(void)
{
  vec_load_bndry ((const signed char *)0, 64);
  __builtin_s390_vll ((unsigned int)0, (const void *)8);
}

__attribute__ ((target("arch=z10")))
void vx0(void)
{
  vec_load_bndry ((const signed char *)0, 64); /* { dg-error "is not supported without -mvx" } */
  __builtin_s390_vll ((unsigned int)0, (const void *)8); /* { dg-error "is not supported without -mvx" } */
}

void vxd(void)
{
  vec_load_bndry ((const signed char *)0, 64);
  __builtin_s390_vll ((unsigned int)0, (const void *)8);
}
