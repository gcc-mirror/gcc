/* Test code generation of vector built-ins.  We don't have this for
   most of ours today.  As new built-ins are added, please add to this
   test case.  Update as necessary to add VSX, P8-vector, P9-vector,
   etc. */

/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec -O0" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

static vector signed int i, *pi;
static int int1;

void
b()
{
  i = __builtin_altivec_lvxl (int1, pi);
  i = vec_lvxl (int1, pi);
}

/* { dg-final { scan-assembler-times "lvxl" 2 } } */
