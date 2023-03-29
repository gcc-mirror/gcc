/* PR target/109109 */
/* { dg-do run { target lp64 } } */
/* { dg-options "-O2" } */

unsigned long arr[64];

__attribute__((noipa)) unsigned __int128
f1 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) arr[ax]) << 64) | ax;
}

__attribute__((noipa)) unsigned __int128
f2 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) arr[dx]) << 64) | ax;
}

__attribute__((noipa)) unsigned __int128
f3 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) ((unsigned long *) (((char *) arr) + ax))[dx]) << 64) | ax;
}

__attribute__((noipa)) unsigned __int128
f4 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) arr[ax]) << 64) | dx;
}

__attribute__((noipa)) unsigned __int128
f5 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) arr[dx]) << 64) | dx;
}

__attribute__((noipa)) unsigned __int128
f6 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) ((unsigned long *) (((char *) arr) + ax))[dx]) << 64) | dx;
}

__attribute__((noipa)) unsigned __int128
f7 (unsigned long di, unsigned long si, unsigned long dx, unsigned long cx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) arr[ax]) << 64) | cx;
}

__attribute__((noipa)) unsigned __int128
f8 (unsigned long di, unsigned long si, unsigned long dx, unsigned long cx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) arr[dx]) << 64) | cx;
}

__attribute__((noipa)) unsigned __int128
f9 (unsigned long di, unsigned long si, unsigned long dx, unsigned long cx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) ((unsigned long *) (((char *) arr) + ax))[dx]) << 64) | cx;
}

__attribute__((noipa)) unsigned __int128
f10 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) ax) << 64) | arr[ax];
}

__attribute__((noipa)) unsigned __int128
f11 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) ax) << 64) | arr[dx];
}

__attribute__((noipa)) unsigned __int128
f12 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) ax) << 64) | ((unsigned long *) (((char *) arr) + ax))[dx];
}

__attribute__((noipa)) unsigned __int128
f13 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) dx) << 64) | arr[ax];
}

__attribute__((noipa)) unsigned __int128
f14 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) dx) << 64) | arr[dx];
}

__attribute__((noipa)) unsigned __int128
f15 (unsigned long di, unsigned long si, unsigned long dx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) dx) << 64) | ((unsigned long *) (((char *) arr) + ax))[dx];
}

__attribute__((noipa)) unsigned __int128
f16 (unsigned long di, unsigned long si, unsigned long dx, unsigned long cx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) cx) << 64) | arr[ax];
}

__attribute__((noipa)) unsigned __int128
f17 (unsigned long di, unsigned long si, unsigned long dx, unsigned long cx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) cx) << 64) | arr[dx];
}

__attribute__((noipa)) unsigned __int128
f18 (unsigned long di, unsigned long si, unsigned long dx, unsigned long cx)
{
  unsigned long ax;
  asm ("" : "=a" (ax) : "0" (di));
  return (((unsigned __int128) cx) << 64) | ((unsigned long *) (((char *) arr) + ax))[dx];
}

int
main ()
{
  for (int i = 0; i < 64; i++)
    arr[i] = 64 + i;
#define CHECK_EQ(x, y1, y2) do { unsigned __int128 y = y1; y <<= 64; y += y2; if (x != y) __builtin_abort (); } while (0)
  CHECK_EQ (f1 (8, 0, 9), 0x48, 0x8);
  CHECK_EQ (f2 (8, 0, 9), 0x49, 0x8);
  CHECK_EQ (f3 (8, 0, 9), 0x4a, 0x8);
  CHECK_EQ (f4 (8, 0, 9), 0x48, 0x9);
  CHECK_EQ (f5 (8, 0, 9), 0x49, 0x9);
  CHECK_EQ (f6 (8, 0, 9), 0x4a, 0x9);
  CHECK_EQ (f7 (8, 0, 9, 10), 0x48, 0xa);
  CHECK_EQ (f8 (8, 0, 9, 10), 0x49, 0xa);
  CHECK_EQ (f9 (8, 0, 9, 10), 0x4a, 0xa);
  CHECK_EQ (f10 (8, 0, 9), 0x8, 0x48);
  CHECK_EQ (f11 (8, 0, 9), 0x8, 0x49);
  CHECK_EQ (f12 (8, 0, 9), 0x8, 0x4a);
  CHECK_EQ (f13 (8, 0, 9), 0x9, 0x48);
  CHECK_EQ (f14 (8, 0, 9), 0x9, 0x49);
  CHECK_EQ (f15 (8, 0, 9), 0x9, 0x4a);
  CHECK_EQ (f16 (8, 0, 9, 10), 0xa, 0x48);
  CHECK_EQ (f17 (8, 0, 9, 10), 0xa, 0x49);
  CHECK_EQ (f18 (8, 0, 9, 10), 0xa, 0x4a);
}
