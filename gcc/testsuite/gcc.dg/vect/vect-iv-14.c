/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-mprefer-vector-width=128" { target { x86_64-*-* i?86-*-* } } } */

#include "tree-vect.h"

#define N 48

#define STORE4(RESULT, VALUE) \
  do { \
    (RESULT)[i++] = (VALUE); \
    (RESULT)[i++] = (VALUE); \
    (RESULT)[i++] = (VALUE); \
    (RESULT)[i++] = (VALUE); \
  } while (0)

static const int expected[N] = {
  /* Round 1.  */
   1,  1,  1,  1,   10, 10, 10, 10,    1,  1,  1,  1,
   1,  1,  1,  1,   10, 10, 10, 10,    1,  1,  1,  1,
  /* Round 2 (x + 2, y + 3).  */
   3,  3,  3,  3,   13, 13, 13, 13,    3,  3,  3,  3,
   3,  3,  3,  3,   13, 13, 13, 13,    3,  3,  3,  3
};

/* To test the multi-IV generation path (CANDIDATE_NIVS > 1), force a
   geometry where CANDIDATE_NIVS < NIVS.  On four-lane integer vectors,
   the minimum SLP group size that satisfies this is 24, creating 6 vectors.

   The first three (X, Y, X) vectors form the first period.  The last three
   form the repeated period, which exercises the IVN % CANDIDATE_NIVS reuse
   path.  */
__attribute__ ((noipa))
void
fill (int *ans, int rounds)
{
  int x = 1;
  int y = 10;
  int i = 0;

  while (rounds > 0)
    {
      /* First period.  */
      STORE4 (ans, x);
      STORE4 (ans, y);
      STORE4 (ans, x);

      /* Repeated period.  */
      STORE4 (ans, x);
      STORE4 (ans, y);
      STORE4 (ans, x);

      --rounds;
      x += 2;
      y += 3;
    }
}

int
main (void)
{
  int ans[N] = { 0 };

  check_vect ();

  fill (ans, 2);

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (ans[i] != expected[i])
      abort ();

  return 0;
}

/* Multi-lane SLP inductions are not yet supported for variable-length
   vectors.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_variable_length } } } */
/* { dg-final { scan-tree-dump-times "reusing 3 SLP induction IVs for 6 vector chunks" 1 "vect" { target { x86_64-*-* i?86-*-* } } } } */
