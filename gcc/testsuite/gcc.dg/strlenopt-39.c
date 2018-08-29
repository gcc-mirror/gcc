/* PR tree-optimization/83444
   { dg-do compile { target i?86-*-* x86_64-*-* } }
   { dg-options "-O2 -fdump-tree-optimized" } */

#include "strlenopt.h"

#define STR "1234567"

const char str[] = STR;

char dst[10];

void copy_from_global_str (void)
{
  strcpy (dst, str);

  if (strlen (dst) != sizeof str - 1)
    abort ();
}

void copy_from_local_str (void)
{
  const char s[] = STR;

  strcpy (dst, s);

  if (strlen (dst) != sizeof s - 1)
    abort ();
}

void copy_from_local_memstr (void)
{
  struct {
    char s[sizeof STR];
  } x = { STR };

  strcpy (dst, x.s);

  if (strlen (dst) != sizeof x.s - 1)
    abort ();
}

void copy_to_local_str (void)
{
  char d[sizeof STR];

  strcpy (d, str);

  if (strlen (d) != sizeof str - 1)
    abort ();
}

void copy_to_local_memstr (void)
{
  struct {
    char d[sizeof STR];
  } x;

  strcpy (x.d, str);

  if (strlen (x.d) != sizeof str- 1)
    abort ();
}

/* Verify that all calls to strlen have been eliminated.
  { dg-final { scan-tree-dump-not "(abort|strlen) \\(\\)" "optimized" } } */
