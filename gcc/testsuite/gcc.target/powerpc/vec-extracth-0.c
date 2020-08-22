/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

int
main (int argc, char *argv [])
{
  vector unsigned char source_a = {
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
  vector unsigned char source_b = {
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 };

  vector unsigned long long int result_1 = { 0, 16 };
  vector unsigned long long int result_2 = { 0, 15 };
  vector unsigned long long int result_3 = { 0, 11 };
  vector unsigned long long int result_4 = { 0, 23 };

  if (!vec_all_eq (vec_extracth (source_a, source_b, 15), result_1))
    abort ();
  if (!vec_all_eq (vec_extracth (source_a, source_b, 16), result_2))
    abort ();
  if (!vec_all_eq (vec_extracth (source_b, source_a, 4), result_3))
    abort ();
  if (!vec_all_eq (vec_extracth (source_b, source_a, 24), result_4))
    abort ();

  return 0;
}

/* { dg-final { scan-assembler {\mvextdubvlx\M} { target le } } } */
/* { dg-final { scan-assembler {\mvextdubvrx\M} { target be } } } */
