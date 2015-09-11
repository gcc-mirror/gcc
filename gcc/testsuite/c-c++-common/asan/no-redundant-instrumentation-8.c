/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

char
foo  (int *a, char *b, char *c)
{
  /* One check for b[0], one check for a[].  */
  __builtin_memmove (c, b, a[b[0]]);
  /* One check for c[0], one check for a[].  */
  __builtin_memmove (b, c, a[c[0]]);
  /* No checks here.  */
  return c[0] + b[0];
  /* For a total of 4 checks.  */
}

/* { dg-final { scan-tree-dump-times "& 7" 4 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load1" 2 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load4" 2 "sanopt" } } */
