/* { dg-do compile { target { powerpc*-*-* } } } */
/* dfp_hw represents power 6 */
/* { dg-require-effective-target dfp_hw } */
/* powerpc_vsx_ok represents power7 */
/* { dg-skip-if "" { powerpc_vsx_ok } } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power6" } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

/* This test follows the pattern of pr78056-2.c, which has been
 * exercised with binutils 2.25.  This test, however, has not
 * been exercised because the author of the test does not have access
 * to a development environment that succesfully bootstraps gcc
 * while at the same lacking assembler support for power 7.  */

/* Though the command line specifies power6 target, this function is
   to support power7.  */
__attribute__((target("cpu=power7")))
int
div_we (int a, int b)
{ /* { dg-warning "lacks power7 support" } */
  return __builtin_divwe (a, b); /* { dg-warning "implicit declaration" } */
}
