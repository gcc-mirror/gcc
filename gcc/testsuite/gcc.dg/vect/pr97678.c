/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_int_mult } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-additional-options "-mavx2" { target avx2_runtime } } */

#include "tree-vect.h"

int
main ()
{
  unsigned int i = 0;
  unsigned short b[158 * 2];

  check_vect ();

  for (i = 0; i < 158; i++)
    {
      b[i * 2] = i * 7;
      b[i * 2 + 1] = i * 8;
    }

#pragma GCC novector
  for (i = 0; i < 158; ++i)
    if (b[i*2] != (unsigned short)(i*7)
        || b[i*2+1] != (unsigned short)(i*8))
      abort ();

  return 0;
}

/* The init loop should be vectorized with SLP.  */
/* We don't yet support SLP inductions for variable length vectors.  */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { xfail vect_variable_length } } } */
