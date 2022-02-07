/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-options "-lm -fno-builtin" } */

/* This testcase ensures that the builtins is correctly expanded and match the
   expected result from the standard function.
   "-fno-builtin" option is used to enable calls to libc implementation of the
   gcc builtins tested when not using __builtin_ prefix. */

#include <fenv.h>

#ifdef DEBUG
#include <stdio.h>
#define FAIL(v, e) printf("ERROR, __builtin_fegetround() returned %d," \
                          " not the expecected value %d\n", v, e);
#else
void abort (void);
#define FAIL(v, e) abort()
#endif

int
main ()
{
  int i, rounding, expected;
  const int rm[] = {FE_TONEAREST, FE_TOWARDZERO, FE_UPWARD, FE_DOWNWARD};
  for (i = 0; i < sizeof rm / sizeof rm[0]; i++)
    {
      fesetround(rm[i]);
      rounding = __builtin_fegetround();
      expected = fegetround();
      if (rounding != expected)
        FAIL(rounding, expected);
    }

  return 0;
}
