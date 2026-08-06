/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* X | -X has the sign bit set exactly when X is non-zero.  */

int f1 (int x)
{ return (x | -x) >> (__SIZEOF_INT__ * __CHAR_BIT__ - 1); }
unsigned int f2 (unsigned int x)
{ return (x | -x) >> (__SIZEOF_INT__ * __CHAR_BIT__ - 1); }
long f3 (long x) { return (x | -x) >> (__SIZEOF_LONG__ * __CHAR_BIT__ - 1); }

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
/* { dg-final { scan-tree-dump-times " != 0" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & " 2 "optimized" } } */
