/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

__SIZE_TYPE__
f (char *a)
{
  a[0] = '1';
  return  __builtin_strlen (a);
}

/* { dg-final { scan-tree-dump-times "__asan_report_load1" 1 "sanopt" } } */
/* { dg-final { cleanup-tree-dump "sanopt" } } */
