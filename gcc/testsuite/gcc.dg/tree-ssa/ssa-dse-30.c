/* PR tree-optimization/80933 - redundant bzero/bcopy calls not eliminated
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-dse1" } */

void sink (void*);

void test_bcopy (const void *s)
{
  char d[33];

  /* Bcopy is transformed into memmove and those calls are expanded
     inline in EVRP, before DSE runs, so this test doesn't actually
     verify that DSE does its job.  */
  __builtin_bcopy (s, d, sizeof d);
  __builtin_bcopy (s, d, sizeof d);

  sink (d);
}

void test_bzero (void)
{
  char d[33];

  __builtin_bzero (d, sizeof d);
  __builtin_bzero (d, sizeof d);

  sink (d);
}

/* { dg-final { scan-tree-dump-times "builtin_memset" 1 "dse1" } } */
/* { dg-final { scan-tree-dump-not "builtin_(bcopy|bzero|memcpy|memmove)" "dse1" } } */
