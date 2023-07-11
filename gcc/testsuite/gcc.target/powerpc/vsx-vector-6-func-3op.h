/* The goal is to have both compile tests which verify the desired instruction
   generation and to functionally test the builtins for correctness.  This is
   done in separate test files, vsx-vector-6-func-3op.c and
   vsx-vector-6-func-3op-run.c.  The vsx-vector-6-func-3op.c test file only
   generates the calls so the instruction counts do not include the counts of
   the instructions generated as part of the result testing.  The result
   checking code differs for BE/LE.  */

#include <altivec.h>
#include <stdio.h>
#include <stdlib.h>

void abort (void);

/* Macro to create call to builtin.  */
#define FLOAT_TEST(NAME)                                                    \
  vector float __attribute__ ((noipa))                                      \
  float_##NAME(vector float f_src_a, vector float f_src_b,                  \
	       vector float f_src_c)					    \
  {									    \
    return vec_##NAME (f_src_a, f_src_b, f_src_c);			    \
  }

FLOAT_TEST (madd)
FLOAT_TEST (msub)

#define DOUBLE_TEST(NAME)                                                    \
  vector double __attribute__ ((noipa))                                      \
  double_##NAME(vector double d_src_a, vector double d_src_b,		     \
		vector double d_src_c)					     \
  {                                                                          \
    return vec_##NAME (d_src_a, d_src_b, d_src_c);			     \
  }

DOUBLE_TEST (madd)
DOUBLE_TEST (msub)

vector unsigned int  __attribute__ ((noipa))
short_msums_unsigned (vector unsigned short us_src_a,
		      vector unsigned short us_src_b,
		      vector unsigned int ui_src_c)
{
   return vec_msums (us_src_a, us_src_b, ui_src_c);
}

vector int __attribute__ ((noipa))
short_msums_signed (vector short ss_src_a, vector short ss_src_b,
	      vector int si_src_c)
{
   return vec_msums (ss_src_a, ss_src_b, si_src_c);
}

vector double __attribute__ ((noipa))
double_sel_test (vector double d_src_a, vector double d_src_b,
		 vector unsigned long long ull_src_c)
{
  return vec_sel (d_src_a, d_src_b, ull_src_c);
 }

vector double __attribute__ ((noipa))
bool_sel_test (vector double d_src_a, vector double d_src_b,
	       vector bool long long bll_src_c)
{
  return vec_sel (d_src_a, d_src_b, bll_src_c);
}

vector double __attribute__ ((noipa))
double_permute_test (vector double d_src_a, vector double d_src_b,
		     vector unsigned char uc_src_c)
{
  return vec_perm (d_src_a, d_src_b, uc_src_c);
}

