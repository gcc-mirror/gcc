/* Copyright (C) 2002  Free Software Foundation.

   Test that optimizing ((c>=1) && (c<=127)) into (signed char)c < 0
   doesn't cause any problems for the compiler and behaves correctly.

   Written by Roger Sayle, 8th May 2002.  */

#include <limits.h>

extern void abort (void);

void
testc (unsigned char c, int ok)
{
  if ((c>=1) && (c<=SCHAR_MAX))
    {
      if (!ok) abort ();
    }
  else
    if (ok) abort ();
}

void
tests (unsigned short s, int ok)
{
  if ((s>=1) && (s<=SHRT_MAX))
    {
      if (!ok) abort ();
    }
  else
    if (ok) abort ();
}

void
testi (unsigned int i, int ok)
{
  if ((i>=1) && (i<=INT_MAX))
    {
      if (!ok) abort ();
    }
  else
    if (ok) abort ();
}

void
testl (unsigned long l, int ok)
{
  if ((l>=1) && (l<=LONG_MAX))
    {
      if (!ok) abort ();
    }
  else
    if (ok) abort ();
}

int
main ()
{
  testc (0, 0);
  testc (1, 1);
  testc (SCHAR_MAX, 1);
  testc (SCHAR_MAX+1, 0);
  testc (UCHAR_MAX, 0);

  tests (0, 0);
  tests (1, 1);
  tests (SHRT_MAX, 1);
  tests (SHRT_MAX+1, 0);
  tests (USHRT_MAX, 0);

  testi (0, 0);
  testi (1, 1);
  testi (INT_MAX, 1);
  testi (INT_MAX+1U, 0);
  testi (UINT_MAX, 0);

  testl (0, 0);
  testl (1, 1);
  testl (LONG_MAX, 1);
  testl (LONG_MAX+1UL, 0);
  testl (ULONG_MAX, 0);

  return 0;
}

