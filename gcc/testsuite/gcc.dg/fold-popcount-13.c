/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/112094 */
/* Verify that popcount comparisons are preserved when they cannot be
   safely simplified.  */

/* The comparison result is used twice.  */
int f_multiuse (unsigned int a, int n, int *out)
{
  int c = __builtin_popcount (a) == n;
  *out = c;
  return (a != 0) | c;
}

extern int get_value (void);
volatile unsigned int value;

/* Both calls to get_value are observable and must be preserved.  */
int f_side_effect (int n)
{
  return (get_value () != 0)
         | (__builtin_popcount (get_value ()) == n);
}

/* Both volatile reads are observable and must be preserved.  */
int f_volatile (int n)
{
  return (value != 0)
         | (__builtin_popcount (value) == n);
}

/* One popcount remains for each negative case.  */
/* { dg-final { scan-tree-dump-times "popcount" 3 "optimized" } } */
