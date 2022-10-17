/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-fre -fdisable-tree-evrp -fdump-tree-vrp1-details -fno-thread-jumps -fdelete-null-pointer-checks" } */

/* Compile with -fno-tree-fre -O2 to prevent CSEing *p.  */
int
foo (int a, int *p)
{
  int x = *p + 2;
  int y = *p - 1;
  int z = *p + 9;

  /* This should be folded to if (1), but only one ASSERT_EXPR should
     be inserted.  */
  if (p)
    a = x + y + z;
  else
    a = x - y;

  return a;
}
/* Target disabling -fdelete-null-pointer-checks should not fold checks */
/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 1" 1 "vrp1" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 1" 0 "vrp1" { target {   keeps_null_pointer_checks } } } } */
