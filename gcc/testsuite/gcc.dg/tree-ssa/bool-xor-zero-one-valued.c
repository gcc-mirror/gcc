/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

_Bool
f1 (unsigned a, unsigned b)
{
  if (a != 0 && a != 1) __builtin_unreachable();
  if (b != 0 && b != 1) __builtin_unreachable();
  return (a == 0) ^ (b != 0);
}

_Bool
f2 (unsigned a, unsigned b)
{
  if (a != 0 && a != 1) __builtin_unreachable();
  if (b != 0 && b != 1) __builtin_unreachable();
  return (a != 0) ^ (b == 0);
}

_Bool
f3 (unsigned a, unsigned b)
{
  if (a != 0 && a != 1) __builtin_unreachable();
  if (b != 0 && b != 1) __builtin_unreachable();
  return (a == 0) ^ (b == 0);
}

_Bool
f4 (unsigned a, unsigned b)
{
  if (a != 0 && a != 1) __builtin_unreachable();
  if (b != 0 && b != 1) __builtin_unreachable();
  return (a != 0) ^ (b != 0);
}

/* { dg-final { scan-tree-dump-times "a_\[0-9\]+\\(D\\) \\^ b_\[0-9\]+\\(D\\)" 4 "optimized" } } */
