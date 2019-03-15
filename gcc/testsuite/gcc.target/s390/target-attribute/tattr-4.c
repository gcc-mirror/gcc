/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-O3 -march=z13 -mno-vx -mno-zvector" } */

#include <vecintrin.h>

#pragma GCC target("no-zvector")
__attribute__ ((target("vx")))
void a1(void)
{
#ifdef __VEC__
#error __VEC__ is defined
#endif
  vec_load_bndry ((const signed char *)0, 64);
  __builtin_s390_vll ((unsigned int)0, (const void *)8);
}
#pragma GCC reset_options

__attribute__ ((target("no-vx")))
void a0(void)
{
#ifdef __VEC__
#error __VEC__ is defined
#endif
  __builtin_s390_vll ((unsigned int)0, (const void *)8); /* { dg-error "requires '-mvx'" } */
}

void d(void)
{
#ifdef __VEC__
#error __VEC__ is defined
#endif
  __builtin_s390_vll ((unsigned int)0, (const void *)8); /* { dg-error "requires '-mvx'" } */
}
