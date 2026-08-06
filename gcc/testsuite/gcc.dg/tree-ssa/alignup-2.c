/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Rounding up by adding the padding is the same as rounding up with a
   mask.  */

unsigned int f1 (unsigned int x) { return x + ((-x) & 15u); }
unsigned int f2 (unsigned int x) { return ((-x) & 4095u) + x; }
unsigned long f3 (unsigned long x) { return x + ((-x) & 63ul); }

/* The identity needs no wrapping type, a signed operand works too.  */
int f5 (int x) { return x + ((-x) & 15); }

unsigned int f6 (unsigned int x, unsigned int *p)
{
  unsigned int pad = (-x) & 15u;
  *p = pad;
  return x + pad;
}

/* Not a power of two, leave it alone.  */
unsigned int f4 (unsigned int x) { return x + ((-x) & 14u); }

/* { dg-final { scan-tree-dump-times " & 14;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 4294967280" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 4294963200" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & -16" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 15" 1 "optimized" } } */
