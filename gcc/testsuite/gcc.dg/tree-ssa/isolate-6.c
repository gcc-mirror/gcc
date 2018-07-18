/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-isolate-paths" } */

int x, y;

static inline int
z ()
{
  return x ? y : 0;
}

int
lower_for (int j)
{
  return j % z ();
}

/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "isolate-paths"} } */

