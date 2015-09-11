/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

void
foo  (int *a, char *b, char *c)
{
  /* One check for a[].  */
  __builtin_memmove (c, b, a[0]);
  /* For a total of 1 checks.  */
  int d = a[0] == 0;
}

/* { dg-final { scan-tree-dump-times "& 7" 1 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load4" 1 "sanopt" } } */
