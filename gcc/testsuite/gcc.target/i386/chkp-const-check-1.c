/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkpopt" } */
/* { dg-final { scan-tree-dump-not "bndcl" "chkpopt" } } */
/* { dg-final { scan-tree-dump-not "bndcu" "chkpopt" } } */

int test (int *p)
{
  p = (int *)__builtin___bnd_set_ptr_bounds (p, sizeof (int));
  return *p;
}
