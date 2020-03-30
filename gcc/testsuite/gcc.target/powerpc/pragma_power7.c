/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=power6 -maltivec -O2" } */

#include <altivec.h>

#pragma GCC target ("cpu=power6,altivec")
#ifdef _ARCH_PWR6
vector int
test1 (vector int a, vector int b)
{
  return vec_add (a, b);
}
#else
#error failed power6 pragma target
#endif

#pragma GCC target ("cpu=power7")
/* Force a re-read of altivec.h with new cpu target. */
#undef _ALTIVEC_H
#include <altivec.h>
#ifdef _ARCH_PWR7
vector signed int
test2 (vector signed int a, vector signed int b)
{
  return vec_sldw (a, b, 3);
}
#else
#error failed to set power7 pragma target
#endif

