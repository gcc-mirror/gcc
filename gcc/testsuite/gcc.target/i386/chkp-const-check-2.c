/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -Wchkp" } */

int test (int *p)
{
  p = (int *)__builtin___bnd_set_ptr_bounds (p, sizeof (int));
  return *(p + 1); /* { dg-warning "memory access check always fail" "" } */
}
