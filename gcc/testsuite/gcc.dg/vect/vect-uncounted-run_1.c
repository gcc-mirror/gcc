/* Check we can derive scalar IV return value from vectorized IV value,
   resetting it on entry into the scalar epilogue loop via BIT_FIELD_REF.  */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

#include <assert.h>
#include "tree-vect.h"

#define N 128
#define VAL 13

__attribute__((noipa, noinline))
int
foo (int *haystack, int needle)
{
  int i = 0;
  while (1)
    {
      if (haystack[i] == needle)
	return i;
      i++;
     }
}

#define CHECK_MATCH(POS) 		\
  void					\
  check_match_ ## POS (void)		\
  {					\
    int input[N] = {[0 ... N-1] = 0};	\
    input[POS] = VAL;			\
    int res = foo (input, VAL);		\
    assert (res == POS);		\
  }

CHECK_MATCH (0)
CHECK_MATCH (3)
CHECK_MATCH (127)

#undef CHECK_MATCH
#define CHECK_MATCH(POS) check_match_ ## POS ()

int
main ()
{
  CHECK_MATCH (0);
  CHECK_MATCH (3);
  CHECK_MATCH (127);
  return 0;
}

/* { dg-final { scan-tree-dump "Loop being analyzed as uncounted." "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
/* Ensure we reset our scalar IV so as to repeat the last vector iteration.  */
/* { dg-final { scan-tree-dump {_[0-9_]+ = BIT_FIELD_REF <vect_i_[0-9_.]+, [0-9]+, 0>} "vect" } } */

