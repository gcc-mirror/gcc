/* Verify zero initialization for VLA automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-tree-gimple" } */

extern void bar (int);

void foo(int n)
{
  int arr[n];
  bar (arr[2]);
  return;
}

/* { dg-final { scan-tree-dump ".DEFERRED_INIT \\(D.\\d*, 2, 1\\)" "gimple" } } */
