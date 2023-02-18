/* PR target/108787 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O2" } */

__attribute__((noipa)) unsigned __int128
foo (unsigned long long x, unsigned long long y, unsigned long long z, unsigned long long u, unsigned long long v, unsigned long long w)
{
  unsigned __int128 r, d;
  r = ((unsigned __int128) x * u);
  d = ((unsigned __int128) y * w);
  r += d;
  d = ((unsigned __int128) z * v);
  r += d;
  return r;
}

int
main ()
{
  if (__CHAR_BIT__ != 8 || __SIZEOF_LONG_LONG__ != 8 || __SIZEOF_INT128__ != 16)
    return 0;
  unsigned __int128 x = foo (0x3efe88da491ULL, 0xd105e9b4a44ULL, 0x4efa677b3dbULL, 0x42c052bac7bULL, 0x99638a13199cULL, 0x56b640d064ULL);
  if ((unsigned long long) (x >> 64) != 0x000000000309ff93ULL
      || (unsigned long long) x != 0xbd5c98fdf2bdbcafULL)
    __builtin_abort ();
  return 0;
}
