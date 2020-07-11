/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

vector unsigned long long int
do_vec_pdep (vector unsigned long long int source,
	     vector unsigned long long int mask) {
  return vec_pdep (source, mask);
}

int
main (int argc, char *argv [])
{
  vector unsigned long long int source_a = { 0xa5f07e3cull, 0x7e3ca5f0ull };
  vector unsigned long long int source_b = { 0x3ca5f07eull, 0x5a0fe7c3ull };

  vector unsigned long long int mask_a = { 0xffff0000ull, 0x0000ffffull };
  vector unsigned long long int mask_b = { 0x0f0f0f0full, 0xf0f0f0f0ull };

  /* See pdepd-0.c for derivation of expected results.

     result_aa [0] is compute (source [0], mask [0];
     result_aa [1] is compute (source [1], mask [1].

     result_ab [0] is compute (source [0], mask [2];
     result_ab [1] is compute (source [1], mask [3].

     result_ba [0] is compute (source [2], mask [0];
     result_ba [1] is compute (source [3], mask [1].

     result_bb [0] is compute (source [2], mask [2];
     result_bb [1] is compute (source [3], mask [3].  */

  vector unsigned long long int result_aa = { 0x7e3c0000ull, 0x0000a5f0ull };
  vector unsigned long long int result_ab = { 0x070e030cull, 0xa050f000ull };
  vector unsigned long long int result_ba = { 0xf07e0000ull, 0x0000e7c3ull };
  vector unsigned long long int result_bb = { 0x0f00070eull, 0xe070c030ull };

  if (!vec_all_eq (do_vec_pdep (source_a, mask_a), result_aa))
    abort ();
  if (!vec_all_eq (do_vec_pdep (source_a, mask_b), result_ab))
    abort ();
  if (!vec_all_eq (do_vec_pdep (source_b, mask_a), result_ba))
    abort ();
  if (!vec_all_eq (do_vec_pdep (source_b, mask_b), result_bb))
    abort ();

  return 0;
}
