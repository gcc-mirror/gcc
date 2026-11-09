/* Test __builtin_clmulg.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

[[gnu::noipa]] unsigned long long
foo (unsigned long long x)
{
  return __builtin_clmulg (-1ULL, x);
}

[[gnu::noipa]] unsigned long long
bar (unsigned long long x)
{
  return __builtin_clmulg (0x123456789abcdef0ULL, x);
}

[[gnu::noipa]] unsigned long long
baz (unsigned long long x)
{
  return __builtin_clmulg (x, -1ULL << 15);
}

[[gnu::noipa]] unsigned long long
qux (unsigned long long x)
{
  return __builtin_clmulg (0xffffULL << 11, x);
}

[[gnu::noipa]] unsigned long long
fred (unsigned long long x)
{
  return __builtin_clmulg (0x3fffULL << 11, x);
}

[[gnu::noipa]] unsigned long long
corge (unsigned long long x, unsigned long long *y)
{
  return __builtin_clmulg (-1ULL, x, y);
}

[[gnu::noipa]] unsigned long long
waldo (unsigned long long x, unsigned long long *y)
{
  return __builtin_clmulg (0x123456789abcdef0ULL, x, y);
}

[[gnu::noipa]] unsigned long long
garply (unsigned long long x, unsigned long long *y)
{
  return __builtin_clmulg (x, -1ULL << 15, y);
}

[[gnu::noipa]] unsigned long long
xyzzy (unsigned long long x, unsigned long long *y)
{
  return __builtin_clmulg (0xffffULL << 11, x, y);
}

[[gnu::noipa]] unsigned long long
plugh (unsigned long long x, unsigned long long *y)
{
  return __builtin_clmulg (0x3fffULL << 11, x, y);
}

int
main ()
{
#if __SIZEOF_LONG_LONG__ * __CHAR_BIT__ == 64
  unsigned long long v = 0xc42a7e1d6b0a2ae7ULL, h, h2;
  if (foo (v) != __builtin_clmulg (v, -1ULL)
      || bar (v) != __builtin_clmulg (v, 0x123456789abcdef0ULL)
      || baz (v) != __builtin_clmulg (v, -1ULL << 15)
      || qux (v) != __builtin_clmulg (v, 0xffffULL << 11)
      || fred (v) != __builtin_clmulg (v, 0x3fffULL << 11)
      || corge (v, &h) != __builtin_clmulg (v, -1ULL, &h2)
      || h != h2
      || waldo (v, &h) != __builtin_clmulg (v, 0x123456789abcdef0ULL, &h2)
      || h != h2
      || garply (v, &h) != __builtin_clmulg (v, -1ULL << 15, &h2)
      || h != h2
      || xyzzy (v, &h) != __builtin_clmulg (v, 0xffffULL << 11, &h2)
      || h != h2
      || plugh (v, &h) != __builtin_clmulg (v, 0x3fffULL << 11, &h2)
      || h != h2)
    __builtin_abort ();
  v = 0x005c7eb5333a35daULL;
  if (foo (v) != __builtin_clmulg (v, -1ULL)
      || bar (v) != __builtin_clmulg (v, 0x123456789abcdef0ULL)
      || baz (v) != __builtin_clmulg (v, -1ULL << 15)
      || qux (v) != __builtin_clmulg (v, 0xffffULL << 11)
      || fred (v) != __builtin_clmulg (v, 0x3fffULL << 11)
      || corge (v, &h) != __builtin_clmulg (v, -1ULL, &h2)
      || h != h2
      || waldo (v, &h) != __builtin_clmulg (v, 0x123456789abcdef0ULL, &h2)
      || h != h2
      || garply (v, &h) != __builtin_clmulg (v, -1ULL << 15, &h2)
      || h != h2
      || xyzzy (v, &h) != __builtin_clmulg (v, 0xffffULL << 11, &h2)
      || h != h2
      || plugh (v, &h) != __builtin_clmulg (v, 0x3fffULL << 11, &h2)
      || h != h2)
    __builtin_abort ();
#endif
}
