/* { dg-options "-O2 -fdump-tree-optimized" } */
_Bool
foo (_Bool a, _Bool b)
{
  int c = 1 - a;
  int d = 1 - b;
  int e = c & d;
  return 1 - e;
}

_Bool
bar (_Bool a, _Bool b)
{
  int c = 1 - a;
  int d = 1 - b;
  _Bool e = c & d;
  return 1 - e;
}

_Bool
baz (_Bool a, _Bool b)
{
  _Bool c = 1 - a;
  _Bool d = 1 - b;
  _Bool e = c & d;
  return 1 - e;
}

/* { dg-final { scan-tree-dump-times "1 - " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "~a" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "~b" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "a_\[0-9\]+.D. \\\| b_\[0-9\]+.D." 3 "optimized" } } */

