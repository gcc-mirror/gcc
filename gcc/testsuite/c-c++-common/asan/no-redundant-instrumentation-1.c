/* This tests that when faced with two references to the same memory
   location in the same basic block, the second reference should not
   be instrumented by the Address Sanitizer.  */

/* { dg-options "-fdump-tree-asan0" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */

extern char tab[4];

static int
test0 ()
{
  /* __builtin___asan_report_store1 called 2 times for the two stores
     below.  */
  tab[0] = 1;
  tab[1] = 2;

  /* __builtin___asan_report_load1 called 1 time for the store
     below.  */
  char t0 = tab[1];

  /* This load should not be instrumented because it is to the same
     memory location as above.  */
  char t1 = tab[1];

  return t0 + t1;
}

__attribute__((noinline, noclone)) static int
test1 (int i)
{
  char foo[4] = {};
  
  /*__builtin___asan_report_store1 called 1 time here to instrument
    the initialization.  */
  foo[i] = 1;

  /*__builtin___asan_report_store1 called 2 times here to instrument
    the store to the memory region of tab.  */
  __builtin_memset (tab, 3, sizeof (tab));

  /* There is no instrumentation for the two memset calls below.  */
  __builtin_memset (tab, 4, sizeof (tab));
  __builtin_memset (tab, 5, sizeof (tab));

  /* There are 2 calls to __builtin___asan_report_store1 and 2 calls
     to __builtin___asan_report_load1 to instrument the store to
     (subset of) the memory region of tab.  */
  __builtin_memcpy (&tab[1], foo + i, 3);

  /* This should not generate a __builtin___asan_report_load1 because
     the reference to tab[1] has been already instrumented above.  */
  return tab[1];

  /* So for these function, there should be 7 calls to
     __builtin___asan_report_store1.  */
}

int
main ()
{
  return test0 () && test1 (0);
}

/* { dg-final { scan-tree-dump-times "__builtin___asan_report_store1" 7 "asan0" } } */
/* { dg-final { scan-tree-dump-times "__builtin___asan_report_load" 2 "asan0" }  } */
/* { dg-final { cleanup-tree-dump "asan0" } } */
