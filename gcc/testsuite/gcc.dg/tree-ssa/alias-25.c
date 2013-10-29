/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

void f (long *p) {
  *p = 42;
  p[4] = 42;
  __builtin_free (p);
}

/* { dg-final { scan-tree-dump-not "= 42" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

