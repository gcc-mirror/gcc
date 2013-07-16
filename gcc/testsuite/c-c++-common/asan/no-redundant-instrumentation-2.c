/* This tests that when faced with two references to the same memory
   location in the same basic block, the second reference should not
   be instrumented by the Address Sanitizer.  But in case of access to
   overlapping regions we must be precise.  */

/* { dg-options "-fdump-tree-asan0" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

int
main ()
{
  char tab[5];

  /* Here, we instrument the access at offset 0 and access at offset
     4.  */
  __builtin_memset (tab, 1, sizeof (tab));
  /* We instrumented access at offset 0 above already, so only access
     at offset 3 is instrumented.  */
  __builtin_memset (tab, 1, 3);
}

/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store1" 3 "asan0" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report" 3 "asan0" }  } */
/* { dg-final { cleanup-tree-dump "asan0" } } */
