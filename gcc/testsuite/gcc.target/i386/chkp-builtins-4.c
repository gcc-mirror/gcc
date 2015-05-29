/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -fdump-tree-chkp" } */
/* { dg-final { scan-tree-dump-not "bnd_null_ptr_bounds" "chkp" } } */

void *
chkp_test (void *p)
{
  return __builtin___bnd_null_ptr_bounds (p);
}
