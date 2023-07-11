/* The goal is to have both compile tests which verify the desired instruction
   generation and to functionally test the builtins for correctness.  This is
   done in separate test files, vsx-vector-6-func-1op.c and
   vsx-vector-6-func-1op-run.c.  The vsx-vector-6-func-1op.c test file only
   generates the calls so the instruction counts do not include the counts
   of the instructions generated as part of the result testing.  The result
   checking code differs for BE/LE.  */

#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

void abort (void);

#define FLOAT_TEST(NAME)                                                  \
  vector float __attribute__ ((noipa))                                    \
  float_##NAME (vector float f_src)                                       \
  {									  \
    return vec_##NAME(f_src);				                  \
  }

FLOAT_TEST (abs)
FLOAT_TEST (ceil)
FLOAT_TEST (floor)
FLOAT_TEST (nearbyint)
FLOAT_TEST (rint)
FLOAT_TEST (trunc)

#define DOUBLE_TEST(NAME)                                                  \
  vector double __attribute__ ((noipa))                                    \
  double_##NAME (vector double d_src)					   \
  {									   \
    return vec_##NAME(d_src);				                   \
  }


DOUBLE_TEST (abs)
DOUBLE_TEST (ceil)
DOUBLE_TEST (floor)
DOUBLE_TEST (nearbyint)
DOUBLE_TEST (rint)
DOUBLE_TEST (trunc)
DOUBLE_TEST (sqrt)
