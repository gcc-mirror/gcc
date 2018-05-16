/* RVRP test Adapted from PR tree-optimization/20702 to test pointer dereference.,

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdisable-tree-evrp -fdump-tree-rvrp-details -fdelete-null-pointer-checks" } */

extern void bar (int);

int
foo (int *p, int b)
{
  int a;

  if (b)
    bar (123);
  else
    bar (321);

  a = *p;
  if (p == 0)
    return 0;

  return a;
}

/* Target disabling -fdelete-null-pointer-checks should not fold checks */
/* { dg-final { scan-tree-dump-times "Branch rewritten" 1 "rvrp" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-times "Branch rewritten" 0 "rvrp" { target {   keeps_null_pointer_checks } } } } */

