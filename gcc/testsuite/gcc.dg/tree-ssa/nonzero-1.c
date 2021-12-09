/* { dg-do compile } */
/* { dg-options "-O2 -fdelete-null-pointer-checks -fdump-tree-optimized" } */
/* { dg-require-weak "" } */

/* { dg-skip-if "" keeps_null_pointer_checks } */
extern int a;
int
t()
{
  /* { dg-final { scan-tree-dump "&a != 0" "optimized" } } */
  return &a!=0;
}
extern int a __attribute__ ((weak));
