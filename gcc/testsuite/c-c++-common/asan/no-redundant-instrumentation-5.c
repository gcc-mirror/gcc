/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

void
foo  (int *a, char *b, char *c)
{
  /* One check for b[0], one check for a[], 2 checks for c and one checks for b.  */
  __builtin_memmove (c, b, a[b[0]]);
  /* For a total of 5 checks.  */
}

/* { dg-final { scan-tree-dump-times "& 7" 5 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load1" 1 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load4" 1 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load_n" 1 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store_n" 1 "sanopt" } } */
/* { dg-final { cleanup-tree-dump "sanopt" } } */
