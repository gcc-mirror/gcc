/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power7" } */
/* { dg-require-effective-target powerpc_altivec } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

/* Test for the byte atomic operations on power8 using lbarx/stbcx.  */
__attribute__((target("cpu=power8")))
char
char_fetch_add_relaxed (char *ptr, int value)
{
  return __atomic_fetch_add (ptr, value, __ATOMIC_RELAXED);
}

/* { dg-final { scan-assembler-times "lbarx" 1 } } */
