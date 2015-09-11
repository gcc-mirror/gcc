/* PR tree-optimization/54471 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

#ifdef __SIZEOF_INT128__
#define T __int128
#else
#define T long long
#endif

void fn1call (void);
void fn2call (void);

void
foo (unsigned T x)
{
  if (x > (unsigned T) -3)
    return;
  unsigned T y = 2 * x;
  if (y == 42)
    fn1call ();
  else
    fn2call ();
}

/* { dg-final { scan-tree-dump "fn1call" "vrp1"} } */
/* { dg-final { scan-tree-dump "fn2call" "vrp1"} } */
