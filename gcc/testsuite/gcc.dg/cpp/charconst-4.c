/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "-Wno-multichar -fsigned-char" } */

/* This tests how overly-long multichar charconsts are truncated, and
   whether "short" multichar charconsts are incorrectly sign extended
   (regardless of char signedness).  Preprocessor is used so that we
   have only one place where the too long warning is generated, so
   that the test works for all targets.

   Neil Booth, 8 May 2002.  */

#include <limits.h>

extern void abort (void);

#if INT_MAX == 32767
# define LONG_CHARCONST '!\234a'
# define SHORT_CHARCONST '\234a'
# define POS_CHARCONST '\1'
#elif INT_MAX == 2147483647
# define LONG_CHARCONST '!\234abc'
# define SHORT_CHARCONST '\234abc'
# define POS_CHARCONST '\234a'
#elif INT_MAX == 9223372036854775807
# define LONG_CHARCONST '!\234abcdefg'
# define SHORT_CHARCONST '\234abcdefg'
# define POS_CHARCONST '\234a'
#else
/* Target int size not handled, do something that won't fail.  */
# define LONG_CHARCONST '\234a'
# define SHORT_CHARCONST '\234a'
# define POS_CHARCONST '\1'
#endif

#if POS_CHARCONST < 0
# error Charconst incorrectly sign-extended
#endif

#if LONG_CHARCONST != SHORT_CHARCONST /* { dg-warning "too long" "" } */
# error Overly long charconst truncates wrongly for preprocessor
#endif

int main ()
{
  if (POS_CHARCONST < 0)
    abort ();
  if (LONG_CHARCONST != SHORT_CHARCONST)  /* { dg-warning "too long" "" } */
    abort ();
  return 0;
}
