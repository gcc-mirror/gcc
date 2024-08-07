/* { dg-do run { target { *-*-linux* && { lp64 && p9vector_hw } } } } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx -save-temps" } */

/* These builtins were not defined until ISA 3.1 but only require ISA 3.0
   support.  */

/* { dg-final { scan-assembler-times {\mvextsb2w\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextsb2d\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextsh2w\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextsh2d\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextsw2d\M} 1 } } */

#include <altivec.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#include <stdlib.h>
#endif

void abort (void);

int main ()
{
  int i;

  vector signed char vec_arg_qi, vec_result_qi;
  vector signed short int vec_arg_hi, vec_result_hi, vec_expected_hi;
  vector signed int vec_arg_wi, vec_result_wi, vec_expected_wi;
  vector signed long long vec_result_di, vec_expected_di;

  /* test sign extend byte to word */
  vec_arg_qi = (vector signed char) {1, 2, 3, 4, 5, 6, 7, 8,
				     -1, -2, -3, -4, -5, -6, -7, -8};

#ifdef __BIG_ENDIAN__
  vec_expected_wi = (vector signed int) {4, 8, -4, -8};
#else
  vec_expected_wi = (vector signed int) {1, 5, -1, -5};
#endif

  vec_result_wi = vec_signexti (vec_arg_qi);

  for (i = 0; i < 4; i++)
    if (vec_result_wi[i] != vec_expected_wi[i]) {
#if DEBUG
      printf("ERROR: vec_signexti(char, int):  ");
      printf("vec_result_wi[%d] != vec_expected_wi[%d]\n",
	     i, i);
      printf("vec_result_wi[%d] = %d\n", i, vec_result_wi[i]);
      printf("vec_expected_wi[%d] = %d\n", i, vec_expected_wi[i]);
#else
      abort();
#endif
    }

  /* test sign extend byte to double */
  vec_arg_qi = (vector signed char){1, 2, 3, 4, 5, 6, 7, 8,
				    -1, -2, -3, -4, -5, -6, -7, -8};

#ifdef __BIG_ENDIAN__
  vec_expected_di = (vector signed long long int){8, -8};
#else
  vec_expected_di = (vector signed long long int){1, -1};
#endif

  vec_result_di = vec_signextll(vec_arg_qi);

  for (i = 0; i < 2; i++)
    if (vec_result_di[i] != vec_expected_di[i]) {
#if DEBUG
      printf("ERROR: vec_signextll(byte, long long int):  ");
      printf("vec_result_di[%d] != vec_expected_di[%d]\n", i, i);
      printf("vec_result_di[%d] = %lld\n", i, vec_result_di[i]);
      printf("vec_expected_di[%d] = %lld\n", i, vec_expected_di[i]);
#else
      abort();
#endif
    }

  /* test sign extend short to word */
  vec_arg_hi = (vector signed short int){1, 2, 3, 4, -1, -2, -3, -4};

#ifdef __BIG_ENDIAN__
  vec_expected_wi = (vector signed int){2, 4, -2, -4};
#else
  vec_expected_wi = (vector signed int){1, 3, -1, -3};
#endif

  vec_result_wi = vec_signexti(vec_arg_hi);

  for (i = 0; i < 4; i++)
    if (vec_result_wi[i] != vec_expected_wi[i]) {
#if DEBUG
      printf("ERROR: vec_signexti(short, int):  ");
      printf("vec_result_wi[%d] != vec_expected_wi[%d]\n", i, i);
      printf("vec_result_wi[%d] = %d\n", i, vec_result_wi[i]);
      printf("vec_expected_wi[%d] = %d\n", i, vec_expected_wi[i]);
#else
      abort();
#endif
    }

  /* test sign extend short to double word */
  vec_arg_hi = (vector signed short int ){1, 3, 5, 7,  -1, -3, -5, -7};

#ifdef __BIG_ENDIAN__
  vec_expected_di = (vector signed long long int){7, -7};
#else
  vec_expected_di = (vector signed long long int){1, -1};
#endif

  vec_result_di = vec_signextll(vec_arg_hi);

  for (i = 0; i < 2; i++)
    if (vec_result_di[i] != vec_expected_di[i]) {
#if DEBUG
      printf("ERROR: vec_signextll(short, double):  ");
      printf("vec_result_di[%d] != vec_expected_di[%d]\n", i, i);
      printf("vec_result_di[%d] = %lld\n", i, vec_result_di[i]);
      printf("vec_expected_di[%d] = %lld\n", i, vec_expected_di[i]);
#else
      abort();
#endif
    }

  /* test sign extend word to double word */
  vec_arg_wi = (vector signed int ){1, 3, -1, -3};

#ifdef __BIG_ENDIAN__
  vec_expected_di = (vector signed long long int){3, -3};
#else
  vec_expected_di = (vector signed long long int){1, -1};
#endif

  vec_result_di = vec_signextll(vec_arg_wi);

  for (i = 0; i < 2; i++)
    if (vec_result_di[i] != vec_expected_di[i]) {
#if DEBUG
      printf("ERROR: vec_signextll(word, double):  ");
      printf("vec_result_di[%d] != vec_expected_di[%d]\n", i, i);
      printf("vec_result_di[%d] = %lld\n", i, vec_result_di[i]);
      printf("vec_expected_di[%d] = %lld\n", i, vec_expected_di[i]);
#else
      abort();
#endif
    }

  return 0;
}
