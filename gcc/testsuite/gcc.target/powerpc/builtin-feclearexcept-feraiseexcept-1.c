/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-options "-lm -fno-builtin" } */

/* This testcase ensures that the builtins expand with the matching arguments
   or otherwise fallback gracefully to a function call, and don't ICE during
   compilation.
   "-fno-builtin" option is used to enable calls to libc implementation of the
   gcc builtins tested when not using __builtin_ prefix. */

#include <fenv.h>

int
main ()
{
  int   rsi = 0;
  long  rsl = 0;
  short rss = 0;
  char  rsc = 0;

  unsigned int   rui = 0;
  unsigned long  rul = 0;
  unsigned short rus = 0;
  unsigned char  ruc = 0;

  int e = FE_DIVBYZERO;

  __builtin_feclearexcept(e);                          // CALL
  __builtin_feclearexcept(FE_ALL_EXCEPT);              // CALL
  __builtin_feclearexcept(FE_INVALID);                 // CALL
  __builtin_feclearexcept(FE_INVALID | FE_INEXACT);    // CALL

  __builtin_feclearexcept(FE_INEXACT | FE_DIVBYZERO |
                          FE_UNDERFLOW | FE_OVERFLOW);  // EXPAND
  __builtin_feclearexcept(FE_INEXACT | FE_OVERFLOW);    // EXPAND
  __builtin_feclearexcept(FE_INEXACT);                  // EXPAND
  __builtin_feclearexcept(FE_DIVBYZERO);                // EXPAND
  __builtin_feclearexcept(FE_UNDERFLOW);                // EXPAND
  __builtin_feclearexcept(FE_OVERFLOW);                 // EXPAND
  __builtin_feclearexcept(0);                           // EXPAND

  rsi = __builtin_feclearexcept(FE_DIVBYZERO);  // EXPAND
  rsl = __builtin_feclearexcept(FE_DIVBYZERO);  // EXPAND
  rss = __builtin_feclearexcept(FE_DIVBYZERO);  // EXPAND
  rsc = __builtin_feclearexcept(FE_DIVBYZERO);  // EXPAND
  rui = __builtin_feclearexcept(FE_DIVBYZERO);  // EXPAND
  rul = __builtin_feclearexcept(FE_DIVBYZERO);  // EXPAND
  rus = __builtin_feclearexcept(FE_DIVBYZERO);  // EXPAND
  ruc = __builtin_feclearexcept(FE_DIVBYZERO);  // EXPAND


  __builtin_feraiseexcept(e);                          // CALL
  __builtin_feraiseexcept(FE_ALL_EXCEPT);              // CALL
  __builtin_feraiseexcept(FE_INVALID);                 // CALL
  __builtin_feraiseexcept(FE_INVALID | FE_INEXACT);    // CALL

  __builtin_feraiseexcept(FE_INEXACT | FE_DIVBYZERO |
                          FE_UNDERFLOW | FE_OVERFLOW);  // EXPAND
  __builtin_feraiseexcept(FE_INEXACT | FE_OVERFLOW);    // EXPAND
  __builtin_feraiseexcept(FE_INEXACT);                  // EXPAND
  __builtin_feraiseexcept(FE_DIVBYZERO);                // EXPAND
  __builtin_feraiseexcept(FE_UNDERFLOW);                // EXPAND
  __builtin_feraiseexcept(FE_OVERFLOW);                 // EXPAND
  __builtin_feraiseexcept(0);                           // EXPAND

  rsi = __builtin_feraiseexcept(FE_DIVBYZERO);  // EXPAND
  rsl = __builtin_feraiseexcept(FE_DIVBYZERO);  // EXPAND
  rss = __builtin_feraiseexcept(FE_DIVBYZERO);  // EXPAND
  rsc = __builtin_feraiseexcept(FE_DIVBYZERO);  // EXPAND
  rui = __builtin_feraiseexcept(FE_DIVBYZERO);  // EXPAND
  rul = __builtin_feraiseexcept(FE_DIVBYZERO);  // EXPAND
  rus = __builtin_feraiseexcept(FE_DIVBYZERO);  // EXPAND
  ruc = __builtin_feraiseexcept(FE_DIVBYZERO);  // EXPAND

  return 0;
}
