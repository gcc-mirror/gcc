/* Copyright (C) 2002  Free Software Foundation.

   Test that (A & C1) op C2 optimizations behave correctly where C1 is
   a constant power of 2, op is == or !=, and C2 is C1 or zero.

   Written by Roger Sayle, 5th May 2002.  */

#include <limits.h>

extern void abort (void);

void test1 (signed char c, int set);
void test2 (unsigned char c, int set);
void test3 (short s, int set);
void test4 (unsigned short s, int set);
void test5 (int i, int set);
void test6 (unsigned int i, int set);
void test7 (long long l, int set);
void test8 (unsigned long long l, int set);

#ifndef LONG_LONG_MAX
#define LONG_LONG_MAX __LONG_LONG_MAX__
#endif
#ifndef LONG_LONG_MIN
#define LONG_LONG_MIN (-LONG_LONG_MAX-1)
#endif
#ifndef ULONG_LONG_MAX
#define ULONG_LONG_MAX (LONG_LONG_MAX * 2ULL + 1)
#endif


void
test1 (signed char c, int set)
{
  if ((c & (SCHAR_MAX+1)) == 0)
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();

  if ((c & (SCHAR_MAX+1)) != 0)
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((c & (SCHAR_MAX+1)) == (SCHAR_MAX+1))
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((c & (SCHAR_MAX+1)) != (SCHAR_MAX+1))
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();
}

void
test2 (unsigned char c, int set)
{
  if ((c & (SCHAR_MAX+1)) == 0)
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();

  if ((c & (SCHAR_MAX+1)) != 0)
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((c & (SCHAR_MAX+1)) == (SCHAR_MAX+1))
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((c & (SCHAR_MAX+1)) != (SCHAR_MAX+1))
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();
}

void
test3 (short s, int set)
{
  if ((s & (SHRT_MAX+1)) == 0)
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();

  if ((s & (SHRT_MAX+1)) != 0)
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((s & (SHRT_MAX+1)) == (SHRT_MAX+1))
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((s & (SHRT_MAX+1)) != (SHRT_MAX+1))
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();
}

void
test4 (unsigned short s, int set)
{
  if ((s & (SHRT_MAX+1)) == 0)
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();

  if ((s & (SHRT_MAX+1)) != 0)
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((s & (SHRT_MAX+1)) == (SHRT_MAX+1))
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((s & (SHRT_MAX+1)) != (SHRT_MAX+1))
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();
}

void
test5 (int i, int set)
{
  if ((i & (INT_MAX+1U)) == 0)
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();

  if ((i & (INT_MAX+1U)) != 0)
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((i & (INT_MAX+1U)) == (INT_MAX+1U))
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((i & (INT_MAX+1U)) != (INT_MAX+1U))
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();
}

void
test6 (unsigned int i, int set)
{
  if ((i & (INT_MAX+1U)) == 0)
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();

  if ((i & (INT_MAX+1U)) != 0)
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((i & (INT_MAX+1U)) == (INT_MAX+1U))
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((i & (INT_MAX+1U)) != (INT_MAX+1U))
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();
}

void
test7 (long long l, int set)
{
  if ((l & (LONG_LONG_MAX+1ULL)) == 0)
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();

  if ((l & (LONG_LONG_MAX+1ULL)) != 0)
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((l & (LONG_LONG_MAX+1ULL)) == (LONG_LONG_MAX+1ULL))
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((l & (LONG_LONG_MAX+1ULL)) != (LONG_LONG_MAX+1ULL))
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();
}

void
test8 (unsigned long long l, int set)
{
  if ((l & (LONG_LONG_MAX+1ULL)) == 0)
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();

  if ((l & (LONG_LONG_MAX+1ULL)) != 0)
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((l & (LONG_LONG_MAX+1ULL)) == (LONG_LONG_MAX+1ULL))
    {
      if (!set) abort ();
    }
  else
    if (set) abort ();

  if ((l & (LONG_LONG_MAX+1ULL)) != (LONG_LONG_MAX+1ULL))
    {
      if (set) abort ();
    }
  else
    if (!set) abort ();
}

int
main ()
{
  test1 (0, 0);
  test1 (SCHAR_MAX, 0);
  test1 (SCHAR_MIN, 1);
  test1 (UCHAR_MAX, 1);

  test2 (0, 0);
  test2 (SCHAR_MAX, 0);
  test2 (SCHAR_MIN, 1);
  test2 (UCHAR_MAX, 1);

  test3 (0, 0);
  test3 (SHRT_MAX, 0);
  test3 (SHRT_MIN, 1);
  test3 (USHRT_MAX, 1);

  test4 (0, 0);
  test4 (SHRT_MAX, 0);
  test4 (SHRT_MIN, 1);
  test4 (USHRT_MAX, 1);

  test5 (0, 0);
  test5 (INT_MAX, 0);
  test5 (INT_MIN, 1);
  test5 (UINT_MAX, 1);

  test6 (0, 0);
  test6 (INT_MAX, 0);
  test6 (INT_MIN, 1);
  test6 (UINT_MAX, 1);

  test7 (0, 0);
  test7 (LONG_LONG_MAX, 0);
  test7 (LONG_LONG_MIN, 1);
  test7 (ULONG_LONG_MAX, 1);

  test8 (0, 0);
  test8 (LONG_LONG_MAX, 0);
  test8 (LONG_LONG_MIN, 1);
  test8 (ULONG_LONG_MAX, 1);

  return 0;
}

