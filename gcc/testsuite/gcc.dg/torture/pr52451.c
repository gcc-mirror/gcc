/* { dg-do run } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-skip-if "fenv" { powerpc-ibm-aix* } } */

#include <fenv.h>

#define TEST_C_NOEX(CMP, S)			\
  r = nan##S CMP arg##S;			\
  if (fetestexcept (FE_INVALID))		\
    __builtin_abort ()

#define TEST_B_NOEX(FN, S)			\
  r = __builtin_##FN (nan##S, arg##S);		\
  if (fetestexcept (FE_INVALID))		\
    __builtin_abort ()

#define TEST_C_EX(CMP, S)			\
  r = nan##S CMP arg##S;			\
  if (!fetestexcept (FE_INVALID))		\
    __builtin_abort ();				\
  feclearexcept (FE_INVALID)

#define TEST(TYPE, S)				\
  volatile TYPE nan##S = __builtin_nan##S ("");	\
  volatile TYPE arg##S = 1.0##S;		\
						\
  TEST_C_NOEX (==, S);				\
  TEST_C_NOEX (!=, S);				\
						\
  TEST_B_NOEX (isgreater, S);			\
  TEST_B_NOEX (isless, S);			\
  TEST_B_NOEX (isgreaterequal, S);		\
  TEST_B_NOEX (islessequal, S);			\
						\
  TEST_B_NOEX (islessgreater, S);		\
  TEST_B_NOEX (isunordered, S);			\
						\
  TEST_C_EX (>, S);				\
  TEST_C_EX (<, S);				\
  TEST_C_EX (>=, S);				\
  TEST_C_EX (<=, S)

int
main (void)
{
  volatile int r;

  feclearexcept (FE_INVALID);

  TEST (float, f);
  TEST (double, );
#if !defined(__hppa__) || !defined(__hpux__)
  /* Long double on hppa*-hpux* is implemented in software and the routines
     in fenv.h do not support it.  */
  TEST (long double, l);
#endif
  
  return 0;
}
