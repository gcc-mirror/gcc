/* Copyright (C) 2003 Free Software Foundation.

   Check that built-in cabs, cabsf and cabsl functions don't
   break anything and produces the expected results.

   Written by Roger Sayle, 1st June 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

#include "builtins-config.h"

extern void link_error(void);

extern float cabsf (float _Complex);
extern double cabs (double _Complex);
extern long double cabsl (long double _Complex);

int
main (void)
{
  /* For each type, test both runtime and compile time (constant folding)
     optimization.  */
  float _Complex fc = 3.0F + 4.0iF;
  double _Complex dc = 3.0 + 4.0i;
  long double _Complex ldc = 3.0L + 4.0iL;

#ifdef HAVE_C99_RUNTIME
  /* Test floats.  */
  if (cabsf (fc) != 5.0F)
    link_error ();
  if (__builtin_cabsf (fc) != 5.0F)
    link_error ();
  if (cabsf (3.0F + 4.0iF) != 5.0F)
    link_error ();
  if (__builtin_cabsf (3.0F + 4.0iF) != 5.0F)
    link_error ();
#endif

  /* Test doubles.  */
  if (cabs (dc) != 5.0)
    link_error ();
  if (__builtin_cabs (dc) != 5.0)
    link_error ();
  if (cabs (3.0 + 4.0i) != 5.0)
    link_error ();
  if (__builtin_cabs (3.0 + 4.0i) != 5.0)
    link_error ();

#ifdef HAVE_C99_RUNTIME
  /* Test long doubles.  */
  if (cabsl (ldc) != 5.0L)
    link_error ();
  if (__builtin_cabsl (ldc) != 5.0L)
    link_error ();
  if (cabsl (3.0L + 4.0iL) != 5.0L)
    link_error ();
  if (__builtin_cabsl (3.0L + 4.0iL) != 5.0L)
    link_error ();
#endif

  return 0;
}

