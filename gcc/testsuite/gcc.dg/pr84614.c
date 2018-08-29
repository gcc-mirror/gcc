/* PR target/84614 */
/* { dg-do run { target int128 } } */
/* { dg-options "-Og -fno-split-wide-types -fno-tree-coalesce-vars -g --param=max-combine-insns=3 -fcompare-debug" } */

unsigned __int128 a;

unsigned __int128
b (unsigned short c, unsigned int d)
{
  unsigned long long e;
  __builtin_sub_overflow (0, d, &e);
  e >>= c;
  c ^= 65535;
  d ^= 824;
  return c + a + d + e;
}

int
main ()
{
  unsigned __int128 x = b (0, 9);
  if (__SIZEOF_INT__ * __CHAR_BIT__ == 32
      && __SIZEOF_LONG_LONG__ * __CHAR_BIT__ == 64
      && __SIZEOF_INT128__ * __CHAR_BIT__ == 128
      && x != (((unsigned __int128) 1 << 64) | 0x10327))
    __builtin_abort ();
  return 0;
}
