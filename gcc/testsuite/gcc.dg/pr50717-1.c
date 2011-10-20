/* PR tree-optimization/50717  */
/* Ensure that widening multiply-and-accumulate is not used where integer
   type promotion or users' casts should prevent it.  */

/* { dg-options "-O2 -fdump-tree-widening_mul" } */

long long
f (unsigned int a, char b, long long c)
{
  return (a * b) + c;
}

int
g (short a, short b, int c)
{
  return (short)(a * b) + c;
}

int
h (char a, char b, int c)
{
  return (char)(a * b) + c;
}

/* { dg-final { scan-tree-dump-times "WIDEN_MULT_PLUS_EXPR" 0 "widening_mul" } } */
/* { dg-final { cleanup-tree-dump "widening_mul" } } */
