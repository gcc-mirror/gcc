/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-local-pure-const-details -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

void *g(unsigned n)
{
  return n ? __builtin_malloc (n) : 0;
}

void *h()
{
  return 0;
}

/* { dg-final { scan-tree-dump "Function found to be malloc: g" "local-pure-const1" } } */
/* { dg-final { scan-tree-dump-not "Function found to be malloc: h" "local-pure-const1" } } */
