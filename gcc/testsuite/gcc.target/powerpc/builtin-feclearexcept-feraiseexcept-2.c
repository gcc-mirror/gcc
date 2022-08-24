/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-options "-lm -fno-builtin" } */

/* This testcase ensures that the builtins are correctly expanded and match the
   expected result.
   "-fno-builtin" option is used to enable calls to libc implementation of the
   gcc builtins tested when not using __builtin_ prefix.
   The excepts parameter needs to be passed as constant to
   __builtin_feclearexcept and __builtin_feraiseexcept because some bultins only
   expand on constant input. */

#include <fenv.h>

#ifdef DEBUG
#include <stdio.h>
#define INFO(...) printf(__VA_ARGS__)
#define FAIL(ret, raised, expected, excepts, excepts_str, func) \
        printf("ERROR [l %d] testing %s (%x): %s returned %d."  \
	       " Raised except bits %x, expecected %x\n",       \
	       __LINE__, excepts_str, excepts, func, ret, raised, expected)
#else
void abort (void);
#define INFO(...)
#define FAIL(ret, raised, expected, excepts, excepts_str, func) abort()
#endif

#define TEST(excepts)                                                          \
    do {                                                                       \
      int ret = 0;                                                             \
      int raised = 0;                                                          \
                                                                               \
      INFO("test: %s (%x)\n", #excepts, excepts);                              \
                                                                               \
      feclearexcept(FE_ALL_EXCEPT);                                            \
      ret = __builtin_feraiseexcept(excepts);                                  \
      raised = fetestexcept(FE_ALL_EXCEPT);                                    \
      if (ret != 0 || raised != (excepts))                                     \
        FAIL(ret, raised, excepts, excepts, #excepts,                          \
	     "__builtin_feraiseexcept");                                       \
                                                                               \
      feraiseexcept(FE_ALL_EXCEPT);                                            \
      ret = __builtin_feclearexcept(excepts);                                  \
      raised = fetestexcept(FE_ALL_EXCEPT);                                    \
      if (ret != 0 || raised != (FE_ALL_EXCEPT & ~(excepts)))                  \
        FAIL(ret, raised, FE_ALL_EXCEPT & ~(excepts), excepts, #excepts,       \
	     "__builtin_feclearexcept");                                       \
    } while (0)

int
main ()
{
    TEST(0);
    TEST(FE_ALL_EXCEPT);

    TEST(FE_INVALID);
    TEST(FE_DIVBYZERO);
    TEST(FE_INEXACT);
    TEST(FE_OVERFLOW);
    TEST(FE_UNDERFLOW);

    TEST(FE_INVALID | FE_DIVBYZERO);
    TEST(FE_INVALID | FE_INEXACT);
    TEST(FE_INVALID | FE_OVERFLOW);
    TEST(FE_INVALID | FE_UNDERFLOW);
    TEST(FE_DIVBYZERO | FE_INEXACT);
    TEST(FE_DIVBYZERO | FE_OVERFLOW);
    TEST(FE_DIVBYZERO | FE_UNDERFLOW);
    TEST(FE_INEXACT | FE_OVERFLOW);
    TEST(FE_INEXACT | FE_UNDERFLOW);
    TEST(FE_OVERFLOW | FE_UNDERFLOW);

    TEST(FE_INVALID | FE_DIVBYZERO | FE_INEXACT);
    TEST(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW);
    TEST(FE_INVALID | FE_DIVBYZERO | FE_UNDERFLOW);
    TEST(FE_INVALID | FE_INEXACT | FE_OVERFLOW);
    TEST(FE_INVALID | FE_INEXACT | FE_UNDERFLOW);
    TEST(FE_INVALID | FE_OVERFLOW | FE_UNDERFLOW);
    TEST(FE_DIVBYZERO | FE_INEXACT | FE_OVERFLOW);
    TEST(FE_DIVBYZERO | FE_INEXACT | FE_UNDERFLOW);
    TEST(FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
    TEST(FE_INEXACT | FE_OVERFLOW | FE_UNDERFLOW);

    TEST(FE_INVALID | FE_DIVBYZERO | FE_INEXACT | FE_UNDERFLOW);
    TEST(FE_INVALID | FE_DIVBYZERO | FE_INEXACT | FE_OVERFLOW);
    TEST(FE_INVALID | FE_DIVBYZERO | FE_UNDERFLOW | FE_OVERFLOW);
    TEST(FE_INVALID | FE_INEXACT | FE_UNDERFLOW | FE_OVERFLOW);
    TEST(FE_DIVBYZERO | FE_INEXACT | FE_UNDERFLOW | FE_OVERFLOW);

  return 0;
}
