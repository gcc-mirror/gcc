/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

void
foo (char *p)
{
  volatile int two = 2;
  __builtin_memcpy (p, "abc", two);
  /* This generates a __builtin___asan_report_store1 because we don't
     optimize previous memcpy call.  */
  p[0] = 'd';
}

/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store1" 1 "sanopt" } } */
