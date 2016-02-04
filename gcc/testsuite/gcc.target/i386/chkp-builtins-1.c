/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -fdump-tree-chkp" } */
/* { dg-final { scan-tree-dump-not "bnd_init_ptr_bounds" "chkp" } } */

void *
chkp_test (void *p)
{
  return __builtin___bnd_init_ptr_bounds (p);
}
