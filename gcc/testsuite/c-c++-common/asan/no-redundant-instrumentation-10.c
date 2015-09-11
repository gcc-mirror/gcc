/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

extern __UINT32_TYPE__ a;

void
foo ()
{
  /* Instrument a with access size 3.  */
  int d = __builtin_memcmp (&a, "123", 3);
  /* This should  generate a __builtin___asan_report_store4, because
     the reference to a has been instrumented above with access size 3.  */
  a = 1;
}

/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store4" 1 "sanopt" } } */
