/* The goal is to have both compile tests which verify the desired instruction
   generation and to functionally test the builtins for correctness.  This is
   done in separate test files, vsx-vector-6-func-cmp.c and
   vsx-vector-6-func-cmp-run.c.  The vsx-vector-6-func-cmp.c test file only
   generates the calls so the instruction counts do not include the counts of
   the instructions generated as part of the result testing.  The result
   checking code differs for BE/LE.  */

#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

void abort (void);

#define FLOAT_TEST(NAME)                                                  \
  vector bool int __attribute__ ((noipa))                                 \
  float_##NAME (vector float f_src_a, vector float f_src_b)		  \
  {                                                                       \
    return vec_##NAME (f_src_a, f_src_b);				  \
  }

FLOAT_TEST (cmpeq)
FLOAT_TEST (cmpgt)
FLOAT_TEST (cmpge)
FLOAT_TEST (cmplt)
FLOAT_TEST (cmple)

#define DOUBLE_TEST(NAME)                                                 \
  vector bool long long __attribute__ ((noipa))                           \
  double_##NAME (vector double d_src_a, vector double d_src_b)		  \
  {								          \
    return vec_##NAME (d_src_a, d_src_b);				  \
  }

DOUBLE_TEST (cmpeq)
DOUBLE_TEST (cmpgt)
DOUBLE_TEST (cmpge)
DOUBLE_TEST (cmplt)
DOUBLE_TEST (cmple)

