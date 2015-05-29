/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

void
foo (char *p)
{
  volatile int zero = 0;
  __builtin_memcpy (p, "abc", zero);
  /* This generates a __builtin___asan_report_store1 because we pass volatile
     zero length into memcpy.  */
  p[0] = 'd';
}

/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store1" 1 "sanopt" } } */
