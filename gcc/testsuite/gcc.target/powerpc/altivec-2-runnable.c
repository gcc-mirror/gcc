/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

/* Endian considerations: The "high" half of a vector with n elements is the
   first n/2 elements of the vector. For little endian, these elements are in
   the rightmost half of the vector. For big endian, these elements are in the
   leftmost half of the vector.  */

int main ()
{
  int i;
  vector bool int vec_bi_arg;
  vector bool long long vec_bll_result, vec_bll_expected;

  vector signed int vec_si_arg;
  vector signed long long int vec_slli_result, vec_slli_expected;
  vector float vec_float_arg;
  vector double vec_double_result, vec_double_expected;

  union conv {
	  double d;
	  unsigned long long l;
  } conv_exp, conv_val;

  /* Use of 'double' and ‘long long’ in AltiVec types requires -mvsx */
  /* __builtin_altivec_vupkhsw and __builtin_altivec_vupklsw
     requires the -mcpu=power8 -mvsx option */

  vec_bi_arg = (vector bool int){ 0, 1, 1, 0 };

  vec_bll_expected = (vector bool long long){ 0, 1 };

  vec_bll_result = vec_unpackh (vec_bi_arg);

  for (i = 0; i < 2; i++) {
    if (vec_bll_expected[i] != vec_bll_result[i])
#if DEBUG
       printf("ERROR: vec_unpackh, vec_bll_expected[%d] = %d does not match vec_bll_result[%d] = %d\n",
	      i, vec_bll_expected[i], i, vec_bll_result[i]);
#else
       abort();
#endif
  }

  vec_bll_expected = (vector bool long long){ 1, 0 };

  vec_bll_result = vec_unpackl (vec_bi_arg);

  for (i = 0; i < 2; i++) {
    if (vec_bll_expected[i] != vec_bll_result[i])
#if DEBUG
       printf("ERROR: vec_unpackl, vec_bll_expected[%d] = %d does not match vec_bll_result[%d] = %d\n",
	      i, vec_bll_expected[i], i, vec_bll_result[i]);
#else
       abort();
#endif
  }


  vec_si_arg = (vector signed int){ 0, 101, 202, 303 };

  vec_slli_expected = (vector signed long long int){ 0, 101 };

  vec_slli_result = vec_unpackh (vec_si_arg);

  for (i = 0; i < 2; i++) {
    if (vec_slli_expected[i] != vec_slli_result[i])
#if DEBUG
       printf("ERROR: vec_unpackh, vec_slli_expected[%d] = %d does not match vec_slli_result[%d] = %d\n",
	      i, vec_slli_expected[i], i, vec_slli_result[i]);
#else
       abort();
#endif
  }

  vec_slli_result = vec_unpackl (vec_si_arg);
  vec_slli_expected = (vector signed long long int){ 202, 303 };

  for (i = 0; i < 2; i++) {
    if (vec_slli_expected[i] != vec_slli_result[i])
#if DEBUG
       printf("ERROR: vec_unpackl, vec_slli_expected[%d] = %d does not match vec_slli_result[%d] = %d\n",
	      i, vec_slli_expected[i], i, vec_slli_result[i]);
#else
       abort();
#endif
  }

  vec_float_arg = (vector float){ 0.0, 1.5, 2.5, 3.5 };

  vec_double_expected = (vector double){ 0.0, 1.5 };

  vec_double_result = vec_unpackh (vec_float_arg);

  for (i = 0; i < 2; i++) {
    if (vec_double_expected[i] != vec_double_result[i])
      {
#if DEBUG
	 printf("ERROR: vec_unpackh(), vec_double_expected[%d] = %f does not match vec_double_result[%d] = %f\n",
		i, vec_double_expected[i], i, vec_double_result[i]);
	 conv_val.d = vec_double_result[i];
	 conv_exp.d = vec_double_expected[i];
	 printf("     vec_unpackh(), vec_double_expected[%d] = 0x%llx does not match vec_double_result[%d] = 0x%llx\n",
		i, conv_exp.l, i,conv_val.l);
#else
	 abort();
#endif
    }
  }

  vec_double_expected = (vector double){ 2.5, 3.5 };

  vec_double_result = vec_unpackl (vec_float_arg);

  for (i = 0; i < 2; i++) {
    if (vec_double_expected[i] != vec_double_result[i])
      {
#if DEBUG
         printf("ERROR: vec_unpackl() vec_double_expected[%d] = %f does not match vec_double_result[%d] = %f\n",
		i, vec_double_expected[i], i, vec_double_result[i]);
	 conv_val.d = vec_double_result[i];
	 conv_exp.d = vec_double_expected[i];
	 printf("     vec_unpackh(), vec_double_expected[%d] = 0x%llx does not match vec_double_result[%d] = 0x%llx\n",
		i, conv_exp.l, i,conv_val.l);
#else
         abort();
#endif
      }
  }

  return 0;
}
