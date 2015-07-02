/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

extern void abort (void);

unsigned int
foo (unsigned int x, unsigned int y)
{
  unsigned int z;

  if (x >= y)
    return 1;

  if (y == x)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
