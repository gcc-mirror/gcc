/* Verify the auto initialization of nested VLA.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern -fdump-tree-gimple" } */
/* { dg-require-effective-target alloca } */

void g(void *);

void foo(int a)
{
  int x[a][a];
  g(x);
}

/* { dg-final { scan-tree-dump ".DEFERRED_INIT \\(D.\\d*, 1, \&\"x\"" "gimple" } } */
