/* { dg-do compile { target freorder } } */
/* { dg-options "-O2 -fdump-tree-optimized-details-blocks -fdump-rtl-bbpart-details-blocks -freorder-blocks-and-partition" } */
volatile int v;
void bar (void) __attribute__((leaf, cold));
void baz (int *);

void
foo (int x, int y, int z)
{
  static int f __attribute__((section ("mysection")));
  f = 1;
  if (__builtin_expect (x, 0))
  if (__builtin_expect (y, 0))
  if (__builtin_expect (z, 0))
    {
      f = 2;
      bar ();
      v += 1;
      v *= 2;
      v /= 2;
      v -= 1;
      v += 1;
      v *= 2;
      v /= 2;
      v -= 1;
      v += 1;
      v *= 2;
      v /= 2;
      v -= 1;
      v += 1;
      v *= 2;
      v /= 2;
      v -= 1;
      v += 1;
      v *= 2;
      v /= 2;
      v -= 1;
      v += 1;
      v *= 2;
      v /= 2;
      v -= 1;
      v += 1;
      v *= 2;
      v /= 2;
      v -= 1;
      v += 1;
      v *= 2;
      v /= 2;
      v -= 1;
      f = 3;
      __builtin_abort ();
    }
  f = 4;
  f = 5;
  baz (&f);
}
/* { dg-final { scan-tree-dump-times "Invalid sum" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "count 0 .precise.," 1 "optimized"} } */
/* { dg-final { scan-rtl-dump-times "COLD_PARTITION" 1 "bbpart"} } */
