/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

void
foo (char *p)
{
  __builtin_memcpy (p, "abc", 0);
  /* This generates a __builtin___asan_report_store1 because we didn't access
     any byte in previous memcpy because of zero length parameter.  */
  p[0] = 'd';
}

/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store1" 1 "sanopt" } } */
