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

/* { dg-final { scan-tree-dump-times "ASAN_" 4 "asan0" } }  */
/* { dg-final { scan-tree-dump "ASAN_CHECK \\(.*, 4\\);" "asan0" } }  */
/* { dg-final { scan-tree-dump "ASAN_CHECK \\(.*, 8\\);" "asan0" } }  */
