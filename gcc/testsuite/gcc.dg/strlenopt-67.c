/* PR tree-optimization/90989 - incorrrect strlen result after second strcpy
   into the same destination.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

// #include "strlenopt.h"

char a[4];

int f4 (void)
{
  char b[4];
  __builtin_strcpy (b, "12");

  int i = __builtin_strcmp (a, b);

  __builtin_strcpy (b, "123");
  if (__builtin_strlen (b) != 3)
    __builtin_abort ();

  return i;
}

int f6 (void)
{
  char b[6];
  __builtin_strcpy (b, "1234");

  int i = __builtin_strcmp (a, b);

  __builtin_strcpy (b, "12345");
  if (__builtin_strlen (b) != 5)
    __builtin_abort ();

  return i;
}

int f8 (void)
{
  char b[8];
  __builtin_strcpy (b, "1234");

  int i = __builtin_strcmp (a, b);

  __builtin_strcpy (b, "1234567");
  if (__builtin_strlen (b) != 7)
    __builtin_abort ();

  return i;
}

/* { dg-final { scan-tree-dump-times "abort|strlen" 0 "optimized" } } */
