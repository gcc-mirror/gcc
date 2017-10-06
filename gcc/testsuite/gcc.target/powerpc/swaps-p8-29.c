/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3 " } */

#include <altivec.h>

extern void abort (void);

const vector char y = { 0, 1, 2, 3,
			4, 5, 6, 7,
			8, 9, 10, 11,
			12, 13, 14, 15 };

vector char
foo (void)
{
  return y;
}

int
main (int argc, char *argv[])
{
  vector char fetched_value = foo ();
  if (fetched_value[0] != 0 || fetched_value[15] != 15)
    abort ();
  else
    return 0;
}
