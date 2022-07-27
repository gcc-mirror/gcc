/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

_Bool foo (_Bool a, _Bool b)
{
  return !a == !b;
}

/* { dg-final { scan-tree-dump "\[ab\]_\[0-9\]+\\(D\\) == \[ba\]_\[0-9\]+\\(D\\)" "forwprop1" } } */
