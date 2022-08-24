/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fsplit-paths" } */

__int128 n;

__attribute__ ((simd)) void
foo (void)
{
  __int128 uninitialized;
  unsigned __int128 *p = &n;

  n >>= *p ? : 2;
  n |= uninitialized;
}
