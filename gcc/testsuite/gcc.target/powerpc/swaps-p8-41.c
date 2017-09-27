/* { dg-require-effective-target p8vector_hw } */
/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3 " } */

#include <altivec.h>

extern void abort (void);

const vector long long int y = { 0, 1 };

vector long long int
foo (void)
{
  return y;
}

int
main (int argc, char *argv[])
{
  vector long long int fetched_value = foo ();
  if (fetched_value[0] != 0 || fetched_value[1] != 1)
    abort ();
  else
    return 0;
}
