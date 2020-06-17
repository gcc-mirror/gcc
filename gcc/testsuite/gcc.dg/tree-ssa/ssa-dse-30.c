/* PR tree-optimization/80933 - redundant bzero/bcopy calls not eliminated
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-dse1" } */

void sink (void*);

void test_bcopy (const void *s)
{
  char d[33];

  /* Bcopy is transformed into memmove before DSE runs, so this test
     doesn't actually verify that DSE does its job.  */
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

/* Merging the evrp folder into substitute_and_fold_engine shuffled
   the order of gimple_fold a bit, so evrp is no longer folding the
   memmove inline.  This folding is instead done by forwprop.  Thus, I
   have remmoved the |memmove in the test below as this is not done
   until after dse.

   What happened was that the propagator engine only called gimple
   fold if replace_uses_in() was successful.  On the other hand, EVRP
   called gimple fold regardless.

   If we really care about previous behavior, we could put a call to
   gimple ::fold_stmt into evrp_folder::fold_stmt().  */
/* { dg-final { scan-tree-dump-not "builtin_(bcopy|bzero|memcpy)" "dse1" } } */
