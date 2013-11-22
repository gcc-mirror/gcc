/* { dg-options "-fdump-tree-asan0" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

char e[200];

struct S
{
  char a[100];
  char b[100];
} s;

int
foo  (int *a, char *b, char *c)
{
  int d = __builtin_memcmp (s.a, e, 100);
  d += __builtin_memcmp (s.a, e, 200);
  return d;
}

/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load" 6 "asan0" } } */
/* { dg-final { scan-tree-dump-not "__builtin___asan_report_store" "asan0" } } */
/* { dg-final { cleanup-tree-dump "asan0" } } */
