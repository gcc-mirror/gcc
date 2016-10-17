/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-combine" } */

int
f1 (int x, int y)
{
  return (x & ~0x0ffff00) | ((y << 8) & 0x0ffff00);
}

int
f2 (int x, int y)
{
  return (x & ~0x0ff000) | ((y & 0x0ff) << 12);
}

int
f3 (int x, int y)
{
  return (x & ~0xffff) | (y & 0xffff);
}

int
f4 (int x, int y)
{
  return (x & ~0xff) | (y & 0xff);
}

long long
f5 (long long x, long long y)
{
  return (x & ~0xffffffffull) | (y & 0xffffffff);
}

/* { dg-final { scan-rtl-dump-times "\\*aarch64_bfi" 5 "combine" } } */
