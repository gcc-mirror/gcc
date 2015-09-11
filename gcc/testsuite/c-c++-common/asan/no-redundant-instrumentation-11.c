/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

extern __UINT32_TYPE__ a;

void
foo ()
{
  /* Instrument a with access size 5.  */
  int d = __builtin_memcmp (&a, "12345", 4);
  /* This should not generate a __builtin___asan_report_store4 because
     the reference to a has been already instrumented above with access
     size 5.  */
  a = 1;
}

/* { dg-final { scan-tree-dump-not "& 7" "sanopt" } } */
/* { dg-final { scan-tree-dump-not "__builtin___asan_report_store" "sanopt" } } */
