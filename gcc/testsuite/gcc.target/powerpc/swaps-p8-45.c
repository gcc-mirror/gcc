/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3 " } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */
/* { dg-final { scan-assembler-not "xxswapd" } } */

#include <altivec.h>

extern void abort (void);

const vector double y = { 0.0, 0.1 };

vector double
foo (void)
{
  return y;
}

int
main (int argc, char *argv[])
{
  vector double fetched_value = foo ();
  if (fetched_value[0] != 0.0 || fetched_value[15] != 0.1)
    abort ();
  else
    return 0;
}
