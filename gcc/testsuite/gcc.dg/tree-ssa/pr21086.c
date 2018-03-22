/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-vrp1 -fdump-tree-dce2 -fdelete-null-pointer-checks" } */

int
foo (int *p)
{
  int a = *p;
  int b = p != 0;

  *p = b;

  if (b)
    return a;
  else
    return 0;
}

/* Target disabling -fdelete-null-pointer-checks should not fold checks */
/* { dg-final { scan-tree-dump "Folding predicate " "vrp1" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-times "Folding predicate " 0 "vrp1" { target {   keeps_null_pointer_checks } } } } */
