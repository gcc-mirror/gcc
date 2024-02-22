/* { dg-do compile { target powerpc64*-*-* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -O2 -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-final { scan-assembler "vcmpgtsd" } } */
/* { dg-final { scan-assembler-not "xxlnor" } } */

/* Test code in simplify-rtx.c that converts
     (!c) != {0,...,0} ? a : b
   into
     c != {0,...,0} ? b : a  */

#include <altivec.h>

volatile vector signed long long x = { 25399, -12900 };
volatile vector signed long long y = { 12178, -9987 };

vector signed long long foo () {
  vector bool long long b = vec_cmpge (x, y);
  vector signed long long z = vec_sel (y, x, b);
  return z;
}
