/* Test *_NORM_MAX macros.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <float.h>

#ifndef FLT_NORM_MAX
#error "FLT_NORM_MAX undefined"
#endif

#ifndef DBL_NORM_MAX
#error "DBL_NORM_MAX undefined"
#endif

#ifndef LDBL_NORM_MAX
#error "LDBL_NORM_MAX undefined"
#endif

extern void abort (void);
extern void exit (int);

int
main (void)
{
  if (FLT_NORM_MAX != FLT_MAX)
    abort ();
  if (DBL_NORM_MAX != DBL_MAX)
    abort ();
#if LDBL_MANT_DIG == 106
  if (LDBL_NORM_MAX != 0x0.ffffffffffffffffffffffffffcp1023L)
    abort ();
#else
  if (LDBL_NORM_MAX != LDBL_MAX)
    abort ();
#endif
  exit (0);
}
