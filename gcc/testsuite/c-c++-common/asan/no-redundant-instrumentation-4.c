/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

void
foo  (int *a, char *b, char *c)
{
  /* One check for c[0], one check for a[].  */
  __builtin_memmove (c, b, a[c[0]]);
  /* For a total of 2 checks.  */
  int d = c[0] == 1;
}

/* { dg-final { scan-tree-dump-times "& 7" 2 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load1" 1 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load4" 1 "sanopt" } } */
