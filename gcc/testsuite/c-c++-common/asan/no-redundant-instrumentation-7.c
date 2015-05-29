/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

char e[5];

extern struct S
{
  int a;
  char b;
} s;

int
foo  (int *a, char *b, char *c)
{
  int d = __builtin_memcmp (&s.a, e, 4);
  /* No check because s.a was instrumented above with access size 4.  */
  return s.a;
}

/* { dg-final { scan-tree-dump-not "& 7" "sanopt" } } */
/* { dg-final { scan-tree-dump-not "__builtin___asan_report_load4" "sanopt" } } */
