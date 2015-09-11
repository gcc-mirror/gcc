/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

void
foo (char *p)
{
  __builtin_memcpy (p, "abc", 2);
  /* This doesn't generate a __builtin___asan_report_store1 because we
     verified p[0] through p[2] is writable in previous memcpy call.  */
  p[0] = 'd';
}

/* { dg-final { scan-tree-dump-not "__builtin___asan_report_store1" "sanopt" } } */
