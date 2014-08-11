/* { dg-options "-fdump-tree-sanopt" } */
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
  /* 2 checks for s.a, 2 checks for e.  */
  int d = __builtin_memcmp (s.a, e, 100);
  /* One check for s.a and one check for e.  */
  d += __builtin_memcmp (s.a, e, 200);
  /* For a total of 6 checks.  */
  return d;
}

/* { dg-final { scan-tree-dump-times "& 7" 6 "sanopt" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load_n" 4 "sanopt" } } */
/* { dg-final { scan-tree-dump-not "__builtin___asan_report_store" "sanopt" } } */
/* { dg-final { cleanup-tree-dump "sanopt" } } */
