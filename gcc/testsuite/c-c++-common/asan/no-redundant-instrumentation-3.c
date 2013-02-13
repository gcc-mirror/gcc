/* { dg-options "-fdump-tree-asan0" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

char
foo (__INT32_TYPE__ *p)
{
  /* This generates a __builtin___asan_report_load1.  */
  __INT32_TYPE__ ret = *(char *) p;
  /* This generates a __builtin___asan_report_store4 depending on the.  */
  *p = 26;
  return ret; 
}

/* { dg-final { scan-tree-dump-times "__builtin___asan_report" 2 "asan0" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load1" 1 "asan0" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store" 1 "asan0" } } */
/* { dg-final { cleanup-tree-dump "asan0" } } */
