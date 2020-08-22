/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

int
main (int argc, char *argv [])
{
  vector unsigned long long source_a = { 0, 14 };
  vector unsigned long long source_b = { 16, 30 };

  vector unsigned long long int result_1 = { 0, 16 };
  vector unsigned long long int result_2 = { 0, 14 };
  vector unsigned long long int result_3 = { 0, 0 };
  vector unsigned long long int result_4 = { 0, 30 };

  if (!vec_all_eq (vec_extracth (source_a, source_b, 8), result_1))
    abort ();
  if (!vec_all_eq (vec_extracth (source_a, source_b, 16), result_2))
    abort ();
  if (!vec_all_eq (vec_extracth (source_b, source_a, 8), result_3))
    abort ();
  if (!vec_all_eq (vec_extracth (source_b, source_a, 16), result_4))
    abort ();

  return 0;
}

/* { dg-final { scan-assembler {\mvextddvlx\M} { target le } } } */
/* { dg-final { scan-assembler {\mvextddvrx\M} { target be } } } */
