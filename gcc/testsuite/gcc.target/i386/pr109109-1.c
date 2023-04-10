/* PR target/109109 */
/* { dg-do run { target ia32 } } */
/* { dg-options "-O2" } */

unsigned int arr[64];

__attribute__((noipa, regparm (2))) unsigned long long
f1 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) arr[ax]) << 32) | ax;
}

__attribute__((noipa, regparm (2))) unsigned long long
f2 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) arr[dx]) << 32) | ax;
}

__attribute__((noipa, regparm (2))) unsigned long long
f3 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) ((unsigned int *) (((char *) arr) + ax))[dx]) << 32) | ax;
}

__attribute__((noipa, regparm (2))) unsigned long long
f4 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) arr[ax]) << 32) | dx;
}

__attribute__((noipa, regparm (2))) unsigned long long
f5 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) arr[dx]) << 32) | dx;
}

__attribute__((noipa, regparm (2))) unsigned long long
f6 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) ((unsigned int *) (((char *) arr) + ax))[dx]) << 32) | dx;
}

__attribute__((noipa, regparm (3))) unsigned long long
f7 (unsigned int ax, unsigned int dx, unsigned int cx)
{
  return (((unsigned long long) arr[ax]) << 32) | cx;
}

__attribute__((noipa, regparm (3))) unsigned long long
f8 (unsigned int ax, unsigned int dx, unsigned int cx)
{
  return (((unsigned long long) arr[dx]) << 32) | cx;
}

__attribute__((noipa, regparm (3))) unsigned long long
f9 (unsigned int ax, unsigned int dx, unsigned int cx)
{
  return (((unsigned long long) ((unsigned int *) (((char *) arr) + ax))[dx]) << 32) | cx;
}

__attribute__((noipa, regparm (2))) unsigned long long
f10 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) ax) << 32) | arr[ax];
}

__attribute__((noipa, regparm (2))) unsigned long long
f11 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) ax) << 32) | arr[dx];
}

__attribute__((noipa, regparm (2))) unsigned long long
f12 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) ax) << 32) | ((unsigned int *) (((char *) arr) + ax))[dx];
}

__attribute__((noipa, regparm (2))) unsigned long long
f13 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) dx) << 32) | arr[ax];
}

__attribute__((noipa, regparm (2))) unsigned long long
f14 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) dx) << 32) | arr[dx];
}

__attribute__((noipa, regparm (2))) unsigned long long
f15 (unsigned int ax, unsigned int dx)
{
  return (((unsigned long long) dx) << 32) | ((unsigned int *) (((char *) arr) + ax))[dx];
}

__attribute__((noipa, regparm (3))) unsigned long long
f16 (unsigned int ax, unsigned int dx, unsigned int cx)
{
  return (((unsigned long long) cx) << 32) | arr[ax];
}

__attribute__((noipa, regparm (3))) unsigned long long
f17 (unsigned int ax, unsigned int dx, unsigned int cx)
{
  return (((unsigned long long) cx) << 32) | arr[dx];
}

__attribute__((noipa, regparm (3))) unsigned long long
f18 (unsigned int ax, unsigned int dx, unsigned int cx)
{
  return (((unsigned long long) cx) << 32) | ((unsigned int *) (((char *) arr) + ax))[dx];
}

int
main ()
{
  for (int i = 0; i < 64; i++)
    arr[i] = 64 + i;
#define CHECK_EQ(x, y) do { if (x != y) __builtin_abort (); } while (0)
  CHECK_EQ (f1 (8, 9), 0x4800000008ULL);
  CHECK_EQ (f2 (8, 9), 0x4900000008ULL);
  CHECK_EQ (f3 (8, 9), 0x4b00000008ULL);
  CHECK_EQ (f4 (8, 9), 0x4800000009ULL);
  CHECK_EQ (f5 (8, 9), 0x4900000009ULL);
  CHECK_EQ (f6 (8, 9), 0x4b00000009ULL);
  CHECK_EQ (f7 (8, 9, 10), 0x480000000aULL);
  CHECK_EQ (f8 (8, 9, 10), 0x490000000aULL);
  CHECK_EQ (f9 (8, 9, 10), 0x4b0000000aULL);
  CHECK_EQ (f10 (8, 9), 0x800000048ULL);
  CHECK_EQ (f11 (8, 9), 0x800000049ULL);
  CHECK_EQ (f12 (8, 9), 0x80000004bULL);
  CHECK_EQ (f13 (8, 9), 0x900000048ULL);
  CHECK_EQ (f14 (8, 9), 0x900000049ULL);
  CHECK_EQ (f15 (8, 9), 0x90000004bULL);
  CHECK_EQ (f16 (8, 9, 10), 0xa00000048ULL);
  CHECK_EQ (f17 (8, 9, 10), 0xa00000049ULL);
  CHECK_EQ (f18 (8, 9, 10), 0xa0000004bULL);
}
