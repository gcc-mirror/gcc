/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* X + (X | -X) clears the lowest set bit of X.  The identity holds for a
   signed operand too, X - 1 overflows only where the source does.  */
unsigned int f4 (unsigned int x) { return x + (x | -x); }
int f5 (int x) { return x + (x | -x); }

unsigned int f6 (unsigned int x, unsigned int *p)
{
  unsigned int y = x | -x;
  *p = y;
  return x + y;
}

/* { dg-final { scan-tree-dump-times " \\| " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & " 2 "optimized" } } */
