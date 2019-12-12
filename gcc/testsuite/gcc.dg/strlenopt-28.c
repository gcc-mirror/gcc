/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

volatile int v;

size_t
f1 (void)
{
  char a[30];
  v += 1;
  memcpy (a, "1234567", 8);
  memcpy (a + 7, "89abcdefg", 10);
  memcpy (a + 16, "h", 2);
  return strlen (a);	// This strlen should be optimized into 17.
}

size_t
f2 (void)
{
  char a[30];
  v += 2;
  strcpy (a, "1234567");
  strcpy (a + 7, "89abcdefg");
  strcpy (a + 16, "h");
  return strlen (a);	// This strlen should be optimized into 17.
}

size_t
f3 (char *a)
{
  v += 3;
  memcpy (a, "1234567", 8);
  memcpy (a + 7, "89abcdefg", 10);
  memcpy (a + 16, "h", 2);
  return strlen (a);	// This strlen should be optimized into 17.
}

size_t
f4 (char *a)
{
  v += 4;
  strcpy (a, "1234567");
  strcpy (a + 7, "89abcdefg");
  strcpy (a + 16, "h");
  return strlen (a);	// This strlen should be optimized into 17.
}

int
main ()
{
  char a[30];
  if (f1 () != 17 || f2 () != 17 || f3 (a) != 17 || f4 (a) != 17)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "strlen \\(" 0 "strlen1" } } */
