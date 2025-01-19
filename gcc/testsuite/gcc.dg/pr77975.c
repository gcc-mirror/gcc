/* PR tree-optimization/77975 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunrolli-details" } */

/* { dg-final { scan-tree-dump-times "Proved that loop 1 iterates 2 times using brute force" 1 "cunrolli" } } */

unsigned int
foo (unsigned int *b)
{
  unsigned int a = 8;
  while (a)
    {
      a += 5;
      a &= 44;
      *b += a;
    }
  return a; 
}

/* { dg-final { scan-tree-dump-times "Proved that loop 1 iterates 3 times using brute force" 1 "cunrolli" } } */

unsigned int
bar (unsigned int *b)
{
  unsigned int a = 3;
  while (a)
    {
      a += 5;
      a &= 44;
      *b += a;
    }
  return a; 
}
