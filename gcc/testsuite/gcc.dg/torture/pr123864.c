/* PR tree-optimization/123864 */
/* { dg-do run { target int128 } } */

int u, v, x;

[[gnu::noipa]] static void
foo ()
{
  __int128 c = (__int128) 0xa5ee4bc88ULL << 64;
  long long b = 0x207b8a7f7LL;
  int a = 4;
  long long y;
  __builtin_add_overflow (a, v, &y);
  c *= y;
  
  unsigned z;
  if (__builtin_add_overflow (b, u, &z))
    z = 0xffffffffU;
  x = __builtin_mul_overflow_p (z, c, c);
}

int
main ()
{
  foo ();
  if (!x)
    __builtin_abort ();
}
