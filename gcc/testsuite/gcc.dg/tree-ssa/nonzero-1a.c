/* { dg-do compile } */
/* { dg-options "-O2 -fdelete-null-pointer-checks -fdump-tree-optimized" } */

/* { dg-skip-if "" keeps_null_pointer_checks } */
extern int a;
int
t()
{
  /* { dg-final { scan-tree-dump-not "&a != 0" "optimized" } } */
  return &a!=0;
}
