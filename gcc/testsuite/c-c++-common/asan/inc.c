/* { dg-options "-fdump-tree-asan0" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

void
foo(int *a)
{
  (*a)++;
}

int
main ()
{
  int a = 0;
  foo (&a);
  return 0;
}

/* { dg-final { scan-tree-dump-times "ASAN_" 1 "asan0" } }  */
/* { dg-final { scan-tree-dump "ASAN_CHECK \\(.*, 4\\);" "asan0" } }  */
/* { dg-final { cleanup-tree-dump "asan0" } } */
