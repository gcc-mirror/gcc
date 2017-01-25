/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power5" } } */

/* powerpc_popcntb_ok represents support for power 5.  */
/* { dg-require-effective-target powerpc_popcntb_ok } */
/* dfp_hw represents support for power 6.  */
/* { dg-skip-if "" { dfp_hw } } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mcpu=power5" } */

/* This test follows the pattern of pr78056-2.c, which has been
 * exercised with binutils 2.25.  This test, however, has not
 * been exercised because the author of the test does not have access
 * to a development environment that succesfully bootstraps gcc
 * while at the same lacking assembler support for power 6.  */

/* This test should succeed on both 32- and 64-bit configurations.  */
/* Though the command line specifies power5 target, this function is
   to support power6. Expect an error message here because this target
   does not support power6.  */
__attribute__((target("cpu=power6")))
/* fabs/fnabs/fsel */
double normal1 (double a, double b)
{ /* { dg-warning "lacks power6 support" } */
  return __builtin_copysign (a, b); /* { dg-warning "implicit declaration" } */
}
