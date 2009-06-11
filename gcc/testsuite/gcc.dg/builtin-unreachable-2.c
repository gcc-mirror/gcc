/* Check that __builtin_unreachable() is a no-return function thus
   causing the dead call to foo() to be removed.  The comparison is
   dead too, and should be removed.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-rtl-cse1" } */
void foo (void);

int
f (int i)
{
  if (i > 1)
    __builtin_unreachable();
  if (i > 1)
    foo ();
  return 1;
}
/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
/* { dg-final { scan-rtl-dump-not "\\(if_then_else" "cse1" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
/* { dg-final { cleanup-rtl-dump "cse1" } } */
