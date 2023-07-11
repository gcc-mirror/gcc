/* The goal is to have both compile tests which verify the desired instruction
   generation and to functionally test the builtins for correctness.  This is
   done in separate test files, vsx-vector-6-func-2lop.c and
   vsx-vector-6-func-2lop-run.c.  The vsx-vector-6-func-2lop.c test file only
   generates the calls so the instruction counts do not include the counts of
   the instructions generated as part of the result testing.  The result
   checking code differs for BE/LE.  */


#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

union conv_t {
  vector float f;
  vector unsigned int u;
  vector double d;
  vector unsigned long ul;
} conv_result, conv_exp, conv_src_a, conv_src_b;

void abort (void);

#define FLOAT_TEST(NAME)                                                    \
  vector float __attribute__ ((noipa))                                      \
  float_##NAME (vector float f_src_a, vector float f_src_b)                 \
  {                                                                         \
    return vec_##NAME (f_src_a, f_src_b);                  		    \
  }

FLOAT_TEST (and)
FLOAT_TEST (andc)
FLOAT_TEST (nor)
FLOAT_TEST (or)
FLOAT_TEST (xor)

#define DOUBLE_TEST(NAME)						     \
  vector double __attribute__ ((noipa))                                      \
  double_##NAME (vector double d_src_a, vector double d_src_b)		     \
  {                                                                          \
    return vec_##NAME (d_src_a, d_src_b);				     \
  }

DOUBLE_TEST (and)
DOUBLE_TEST (andc)
DOUBLE_TEST (nor)
DOUBLE_TEST (or)
DOUBLE_TEST (xor)
