/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

void
foo  (int *a, char *b, char *c)
{
  /* One check for c[0], one check for a[], one check for c and 2 checks for b.  */
  __builtin_memmove (c, b, a[c[0]]);
  /* One check for a[], one check for c and one check for b.  */
  __builtin_memmove (c, b, a[b[0]]);
  /* For a total of 8 checks.  */
}

/* { dg-final { scan-tree-dump-times "& 7" 8 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load1" 1 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load4" 2 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load_n" 2 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store_n" 2 "sanopt" } } */
/* { dg-final { cleanup-tree-dump "sanopt" } } */
