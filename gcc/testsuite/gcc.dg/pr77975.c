/* PR tree-optimization/77975 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivcanon-details" } */

/* { dg-final { scan-tree-dump-times "Proved that loop 1 iterates 1 times using brute force" 1 "ivcanon" } } */

unsigned int
foo (unsigned int *b)
{
  unsigned int a = 3;
  while (a)
    {
      a >>= 1;
      *b += a;
    }
  return a; 
}

/* { dg-final { scan-tree-dump-times "Proved that loop 1 iterates 2 times using brute force" 1 "ivcanon" } } */

unsigned int
bar (unsigned int *b)
{
  unsigned int a = 7;
  while (a)
    {
      a >>= 1;
      *b += a;
    }
  return a; 
}
