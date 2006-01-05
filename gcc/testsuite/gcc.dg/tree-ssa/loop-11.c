/* A test for final value replacement and higher-order ivs,
   see PR 22442.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

void bar (unsigned);

void foo (void)
{
  unsigned i, a;

  for (i = 0; i < 5; i++)
    a = i * i;

  bar (a);
}

/* Final value of a gets replaced.  */

/* { dg-final { scan-tree-dump-times "\\(16\\)" 1 "optimized" } } */

/* And the empty loop is removed.  */

/* { dg-final { scan-tree-dump-times "if " 0 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */

