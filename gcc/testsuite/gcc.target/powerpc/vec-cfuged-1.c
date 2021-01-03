/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

vector unsigned long long int
do_vec_cfuge (vector unsigned long long int source,
	      vector unsigned long long int mask)
{
  return vec_cfuge (source, mask);
}

int
vectors_equal (vector unsigned long long int a,
	       vector unsigned long long int b)
{
  return (a[0] == b[0]) && (a[1] == b[1]);
}

int main (int argc, char *argv [])
{
  vector unsigned long long int source_a = { 0xa5f07e3cull, 0x7e3ca5f0ull };
  vector unsigned long long int source_b = { 0x3ca5f07eull, 0x5a0fe7c3ull };

  vector unsigned long long int mask_a = { 0xffff0000ull, 0x0000ffffull };
  vector unsigned long long int mask_b = { 0x0f0f0f0full, 0xf0f0f0f0ull };

  /* See cfuged-0.c for derivation of expected results.

     result_aa [0] is compute (source [0], mask [0];
     result_aa [1] is compute (source [1], mask [1].

     result_ab [0] is compute (source [0], mask [2];
     result_ab [1] is compute (source [1], mask [3].

     result_ba [0] is compute (source [2], mask [0];
     result_ba [1] is compute (source [3], mask [1].

     result_bb [0] is compute (source [2], mask [2];
     result_bb [1] is compute (source [3], mask [3].  */

  vector unsigned long long int result_aa = { 0x7e3ca5f0ull, 0x7e3ca5f0ull };
  vector unsigned long long int result_ab = { 0xaf7350ecull, 0xec5073afull };
  vector unsigned long long int result_ba = { 0xf07e3ca5ull, 0x5a0fe7c3ull };
  vector unsigned long long int result_bb = { 0x3af7c50eull, 0xaf7350ecull };

  if (!vectors_equal (do_vec_cfuge (source_a, mask_a), result_aa))
    abort ();
  if (!vectors_equal (do_vec_cfuge (source_a, mask_b), result_ab))
    abort ();
  if (!vectors_equal (do_vec_cfuge (source_b, mask_a), result_ba))
    abort ();
  if (!vectors_equal (do_vec_cfuge (source_b, mask_b), result_bb))
    abort ();

  return 0;
}
