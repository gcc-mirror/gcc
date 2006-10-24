/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

double foo (double x)
{
  return __builtin_floor (__builtin_fabs (x));
}

long bar (double x)
{
  return __builtin_lfloor (__builtin_fabs (x));
}

/* { dg-final { scan-tree-dump-not "lfloor" "gimple" } } */
/* { dg-final { scan-tree-dump "trunc" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
