/* PR tree-optimization/83671 - fix for false positive reported by
   -Wstringop-overflow does not work with inlining
   Verify that the length the empty string is folded to zero even at -O1
   regardless of offset into it.
   Also verify that the length of a non-empty string isn't folded given
   a variable offset.
   { dg-do compile }
   { dg-options "-O1 -fdump-tree-optimized" } */

#include "strlenopt.h"

inline unsigned length (const char *s)
{
  return __builtin_strlen (s);
}

void check_length_cst (int i)
{
  unsigned len = length (&""[i]);

  if (len)
    __builtin_abort ();
}

void check_length_var (int i)
{
  unsigned len = length (&"1"[i]);

  if (len != 1)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-times "abort" 1 "optimized" } }
   { dg-final { scan-tree-dump-times "strlen" 1 "optimized" } } */
