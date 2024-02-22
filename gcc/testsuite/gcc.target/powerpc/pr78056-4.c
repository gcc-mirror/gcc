/* { dg-do compile { target { powerpc*-*-* } } } */
/* powerpc_vsx_ok represents power7 */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "" { powerpc_vsx_ok } } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power7" } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

/* Though the command line specifies power7 target, this function is
   to support power8, which will fail because this platform does not
   support power8.  */
__attribute__((target("cpu=power8")))
char
char_fetch_add_relaxed (char *ptr, int value)
{ /* { dg-warning "lacks power8 support" } */
  return __atomic_fetch_add (ptr, value, __ATOMIC_RELAXED);
}
