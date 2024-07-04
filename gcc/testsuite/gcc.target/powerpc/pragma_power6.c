/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power6 -maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

#pragma GCC target ("cpu=power6,altivec")
#ifdef _ARCH_PWR6
vector int
isa_2_05 (vector int a, vector int b)
{
  return vec_add (a, b);
}
#else
#error failed power6 pragma target
#endif

