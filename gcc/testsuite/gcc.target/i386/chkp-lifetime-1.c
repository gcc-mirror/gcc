/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkpopt-details" } */
/* { dg-final { scan-tree-dump "Moving creation of \[^ \]+ down to its use" "chkpopt" } } */

extern int arr[];

int test (int i)
{
  int res;
  if (i >= 0)
    res = arr[i];
  else
    res = -i;
  return res;
}
