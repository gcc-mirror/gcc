/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fdelete-null-pointer-checks" } */
static void bad_boy()
{
}
int
main()
{
  void *t=(void *)bad_boy;
  if (!t)
    return 1;
  return 0;
}
/* { dg-final { scan-tree-dump-not "bad_boy" "optimized" { target { ! keeps_null_pointer_checks } } } } */
