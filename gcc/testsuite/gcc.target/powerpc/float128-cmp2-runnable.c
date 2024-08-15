/* { dg-do run } */
/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 " } */

#define NAN_Q __builtin_nanq ("")
#define SNAN_Q __builtin_nansq ("")
#define NAN __builtin_nan ("")
#define SNAN __builtin_nans ("")

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int main(void)
{
  int result;
  double a_dble, b_dble;
  __ieee128 a_ieee128, b_ieee128;
  
  a_dble = 3.10;
  b_dble = 3.10;
  
  if (__builtin_vec_scalar_cmp_exp_eq(a_dble, b_dble))
#ifdef DEBUG
    printf("Double EQ result is true, expecting true\n");
#else
    ;
#endif
  else
#ifdef DEBUG
    printf("ERROR: Double EQ result is false, expecting true\n");
#else
    abort();
#endif

  a_dble = 3.10;
  b_dble = 31.0;
  
  if (__builtin_vec_scalar_cmp_exp_eq(a_dble, b_dble))
#ifdef DEBUG
    printf("ERROR: Double EQ result is true, expecting false\n");
#else
    abort();
#endif
  else
#ifdef DEBUG
    printf("Double EQ result is false, expecting false\n");
#else
    ;
#endif

  a_dble = 3.10;
  b_dble = 3.10;

  if (__builtin_vec_scalar_cmp_exp_lt(a_dble, b_dble))
#ifdef DEBUG
    printf("ERROR: Double LT result is true, expecting false\n");
#else
    abort();
#endif
  else
#ifdef DEBUG
    printf("Double LT result is false, expecting false\n");
#else
    ;
#endif

  a_dble = 0.31;
  b_dble = 3.10;
  
  if (__builtin_vec_scalar_cmp_exp_lt(a_dble, b_dble))
#ifdef DEBUG
    printf("Double LT result is true, expecting true\n");
#else
    ;
#endif
  else
#ifdef DEBUG
    printf("ERROR: Double LT result is false, expecting true\n");
#else
    abort();
#endif

  a_dble = 0.31;
  b_dble = 3.10;

  if (__builtin_vec_scalar_cmp_exp_gt(a_dble, b_dble))
#ifdef DEBUG
    printf("ERROR: Double GT result is true, expecting false\n");
#else
    abort();
#endif
  else
#ifdef DEBUG
    printf("Double GT result is false, expecting false\n");
#else
    ;
#endif

  a_dble = 3.10;
  b_dble = 0.31;
  
  if (__builtin_vec_scalar_cmp_exp_gt(a_dble, b_dble))
#ifdef DEBUG
    printf("Double GT result is true, expecting true\n");
#else
    ;
#endif
  else
#ifdef DEBUG
    printf("ERROR: Double GT result is false, expecting true\n");
#else
    abort();
#endif

  a_dble = NAN;
  b_dble = NAN;
  
  if (__builtin_vec_scalar_cmp_exp_unordered(a_dble, b_dble))
#ifdef DEBUG
    printf("Double unordered result is true, expecting true\n");
#else
    ;
#endif
  else
#ifdef DEBUG
    printf("ERROR: Double unordered result is false, expecting true\n");
#else
    abort();
#endif

  a_dble = 3.10;
  b_dble = 3.10;
  
  if (__builtin_vec_scalar_cmp_exp_unordered(a_dble, b_dble))
#ifdef DEBUG
    printf("ERROR: Double unordered result is true, expecting false\n");
#else
    abort();
#endif
  else
#ifdef DEBUG
    printf("Double unordered result is false, expecting false\n");
#else
    ;
#endif
    
  /* IEEE 128 */
  a_ieee128 = 3.10;
  b_ieee128 = 3.10;
  
  if (__builtin_vec_scalar_cmp_exp_eq(a_ieee128, b_ieee128))
#ifdef DEBUG
    printf("IEEE 128 EQ result is true, expecting true\n");
#else
    ;
#endif
  else
#ifdef DEBUG
    printf("ERROR: IEEE 128 EQ result is false, expecting true\n");
#else
    abort();
#endif

  a_ieee128 = 3.10;
  b_ieee128 = 31.0;
  
  if (__builtin_vec_scalar_cmp_exp_eq(a_ieee128, b_ieee128))
#ifdef DEBUG
    printf("ERROR: IEEE 128 EQ result is true, expecting false\n");
#else
    abort();
#endif
  else
#ifdef DEBUG
    printf("IEEE 128 EQ result is false, expecting false\n");
#else
    ;
#endif

  a_ieee128 = 3.10;
  b_ieee128 = 3.10;

  if (__builtin_vec_scalar_cmp_exp_lt(a_ieee128, b_ieee128))
#ifdef DEBUG
    printf("ERROR: IEEE 128 LT result is true, expecting false\n");
#else
    abort();
#endif
  else
#ifdef DEBUG
    printf("IEEE 128 LT result is false, expecting false\n");
#else
    ;
#endif

  a_ieee128 = 0.31;
  b_ieee128 = 3.10;
  
  if (__builtin_vec_scalar_cmp_exp_lt(a_ieee128, b_ieee128))
#ifdef DEBUG
    printf("IEEE 128 LT result is true, expecting true\n");
#else
    ;
#endif
  else
#ifdef DEBUG
    printf("ERROR: IEEE 128 LT result is false, expecting true\n");
#else
    abort();
#endif

  a_ieee128 = 0.31;
  b_ieee128 = 3.10;

  if (__builtin_vec_scalar_cmp_exp_gt(a_ieee128, b_ieee128))
#ifdef DEBUG
    printf("ERROR: IEEE 128 GT result is true, expecting false\n");
#else
    abort();
#endif
  else
#ifdef DEBUG
    printf("IEEE 128 GT result is false, expecting false\n");
#else
    ;
#endif

  a_ieee128 = 3.10;
  b_ieee128 = 0.31;
  
  if (__builtin_vec_scalar_cmp_exp_gt(a_ieee128, b_ieee128))
#ifdef DEBUG
    printf("IEEE 128 GT result is true, expecting true\n");
#else
    ;
#endif
  else
#ifdef DEBUG
    printf("ERROR: IEEE 128 GT result is false, expecting true\n");
#else
    abort();
#endif

  a_ieee128 = NAN_Q;
  b_ieee128 = NAN_Q;
  
  if (__builtin_vec_scalar_cmp_exp_unordered(a_ieee128, b_ieee128))
#ifdef DEBUG
    printf("IEEE unordered result is true, expecting true\n");
#else
    ;
#endif
  else
#ifdef DEBUG
    printf("ERROR: IEEE unordered result is false, expecting true\n");
#else
    abort();
#endif

  a_ieee128 = 3.10;
  b_ieee128 = 3.10;
  
  if (__builtin_vec_scalar_cmp_exp_unordered(a_ieee128, b_ieee128))
#ifdef DEBUG
    printf("ERROR: IEEE unordered result is true, expecting false\n");
#else
    abort();
#endif
  else
#ifdef DEBUG
    printf("IEEE unordered result is false, expecting false\n");
#else
    ;
#endif
}
