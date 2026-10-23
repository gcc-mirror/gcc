/* ORDERED and NE do not raise FE_INVALID for a quiet NaN but LTGT does,
   so without -fno-trapping-math they must not be combined.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options ieee } */

int is_ordered_and_nonequal_1 (double a, double b)
{
  return !__builtin_isunordered (a, b) & (a != b);
}

int is_ordered_and_nonequal_2 (double a, double b)
{
  return (b != a) & !__builtin_isunordered (a, b);
}

int is_ordered_and_nonequal_3 (double a, double b)
{
  return !__builtin_isunordered (a, b) && (a != b);
}

int is_ordered_or_nonequal_4 (double a, double b)
{
  return !(__builtin_isunordered (a, b) || (a == b));
}

/* { dg-final { scan-tree-dump-not "<>" "optimized" } } */
