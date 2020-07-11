/* { dg-options "-mdejagnu-cpu=power10" } */
/* { dg-additional-options "-mbig" { target powerpc64le-*-* } } */

#include <altivec.h>

extern void abort (void);

int
main (int argc, char *argv [])
{
  vector unsigned short source_a = { 0, 2, 4, 6, 8, 10, 12, 14 };
  vector unsigned short source_b = { 16, 18, 20, 22, 24, 26, 28, 30 };

  vector unsigned long long int result_1 = { 0, 16 };
  vector unsigned long long int result_2 = { 0, 14 };
  vector unsigned long long int result_3 = { 0, 6 };
  vector unsigned long long int result_4 = { 0, 18 };

  if (!vec_all_eq (vec_extracth (source_a, source_b, 14), result_1))
    abort ();
  if (!vec_all_eq (vec_extracth (source_a, source_b, 16), result_2))
    abort ();
  if (!vec_all_eq (vec_extracth (source_b, source_a, 8), result_3))
    abort ();
  if (!vec_all_eq (vec_extracth (source_b, source_a, 28), result_4))
    abort ();

  return 0;
}

/* { dg-final { scan-assembler {\mvextduhvrx\M} } } */
