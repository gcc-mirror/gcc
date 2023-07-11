/* The goal is to have both compile tests which verify the desired instruction
   generation and to functionally test the builtins for correctness.  This is
   done in separate test files, vsx-vector-6-func-2op.c and
   vsx-vector-6-func-2op-run.c.  The vsx-vector-6-func-2opc test file only
   generates the calls so the instruction counts do not include the counts of
   the instructions generated as part of the result testing.  The result
   checking code differs for BE/LE.  */


#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

void abort (void);

#define FLOAT_TEST(NAME)						    \
  vector float __attribute__ ((noipa))                                      \
  float_##NAME(vector float f_src_a, vector float f_src_b)		    \
  {									    \
    return vec_##NAME (f_src_a, f_src_b);		                    \
  }

FLOAT_TEST (add)
FLOAT_TEST (div)
FLOAT_TEST (max)
FLOAT_TEST (min)
FLOAT_TEST (mul)
FLOAT_TEST (sub)

#define DOUBLE_TEST(NAME)						    \
  vector double __attribute__ ((noipa))                                     \
  double_##NAME(vector double d_src_a, vector double d_src_b)               \
  {									    \
    return vec_##NAME (d_src_a, d_src_b);		                    \
  }

DOUBLE_TEST (add)
DOUBLE_TEST (div)
DOUBLE_TEST (max)
DOUBLE_TEST (min)
DOUBLE_TEST (mul)
DOUBLE_TEST (sub)
