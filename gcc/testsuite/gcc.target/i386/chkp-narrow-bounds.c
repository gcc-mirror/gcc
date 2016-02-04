/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkp" } */
/* { dg-final { scan-tree-dump "bndcl" "chkp" } } */
/* { dg-final { scan-tree-dump "bndcu" "chkp" } } */

int
test (int *p)
{
  int *p1 = __bnd_narrow_ptr_bounds (p - 10, p, sizeof (int) * 20);
  return p1[10];
}
