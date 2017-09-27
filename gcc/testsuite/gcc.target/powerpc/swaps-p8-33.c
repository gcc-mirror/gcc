/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3 " } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */
/* { dg-final { scan-assembler-not "xxswapd" } } */

#include <altivec.h>

extern void abort (void);

const vector short y = { 0, 1, 2, 3,
			 4, 5, 6, 7 };

vector short
foo (void)
{
  return y;
}

int
main (int argc, char *argv[])
{
  vector short fetched_value = foo ();
  if (fetched_value[0] != 0 || fetched_value[15] != 15)
    abort ();
  else
    return 0;
}
