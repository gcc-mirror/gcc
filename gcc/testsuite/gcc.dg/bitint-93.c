/* PR middle-end/114073 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -Wno-psabi" } */
/* { dg-additional-options "-mavx512f" { target i?86-*-* x86_64-*-* } } */

typedef int V __attribute__((vector_size (sizeof (_BitInt(256)))));
typedef int W __attribute__((vector_size (sizeof (_BitInt(512)))));

#if __BITINT_MAXWIDTH__ >= 256 && defined (__SIZEOF_INT128__)
_Complex __int128
f1 (_BitInt(256) x)
{
  union U { _BitInt(256) x; _Complex __int128 y; } u;
  u.x = x;
  return u.y;
}

_Complex __int128
f2 (_BitInt(254) x)
{
  union U { _BitInt(254) x; _Complex __int128 y; } u;
  u.x = x;
  return u.y;
}

_BitInt(256)
f3 (_Complex __int128 x)
{
  union U { _BitInt(256) x; _Complex __int128 y; } u;
  u.y = x;
  return u.x;
}

_BitInt(252)
f4 (_Complex __int128 x)
{
  union U { _BitInt(252) x; _Complex __int128 y; } u;
  u.y = x;
  return u.x;
}

_Complex __int128
f5 (_BitInt(256) x)
{
  union U { _BitInt(256) x; _Complex __int128 y; } u;
  u.x = x + 1;
  return u.y;
}

_Complex __int128
f6 (_BitInt(254) x)
{
  union U { _BitInt(254) x; _Complex __int128 y; } u;
  u.x = x + 1;
  return u.y;
}

_Complex __int128
f7 (_BitInt(256) *x)
{
  union U { _BitInt(256) x; _Complex __int128 y; } u;
  u.x = *x + 1;
  return u.y;
}

_Complex __int128
f8 (_BitInt(254) *x)
{
  union U { _BitInt(254) x; _Complex __int128 y; } u;
  u.x = *x + 1;
  return u.y;
}

_BitInt(256)
f9 (_Complex __int128 x)
{
  union U { _BitInt(256) x; _Complex __int128 y; } u;
  u.y = x;
  return u.x + 1;
}

_BitInt(252)
f10 (_Complex __int128 x)
{
  union U { _BitInt(252) x; _Complex __int128 y; } u;
  u.y = x;
  return u.x + 1;
}
#endif

#if __BITINT_MAXWIDTH__ >= 256
V
f11 (_BitInt(256) x)
{
  union U { _BitInt(256) x; V y; } u;
  u.x = x;
  return u.y;
}

V
f12 (_BitInt(254) x)
{
  union U { _BitInt(254) x; V y; } u;
  u.x = x;
  return u.y;
}

_BitInt(256)
f13 (V x)
{
  union U { _BitInt(256) x; V y; } u;
  u.y = x;
  return u.x;
}

_BitInt(252)
f14 (V x)
{
  union U { _BitInt(252) x; V y; } u;
  u.y = x;
  return u.x;
}

V
f15 (_BitInt(256) x)
{
  union U { _BitInt(256) x; V y; } u;
  u.x = x + 1;
  return u.y;
}

V
f16 (_BitInt(254) x)
{
  union U { _BitInt(254) x; V y; } u;
  u.x = x + 1;
  return u.y;
}

V
f17 (_BitInt(256) *x)
{
  union U { _BitInt(256) x; V y; } u;
  u.x = *x + 1;
  return u.y;
}

V
f18 (_BitInt(254) *x)
{
  union U { _BitInt(254) x; V y; } u;
  u.x = *x + 1;
  return u.y;
}

_BitInt(256)
f19 (V x)
{
  union U { _BitInt(256) x; V y; } u;
  u.y = x;
  return u.x + 1;
}

_BitInt(252)
f20 (V x)
{
  union U { _BitInt(252) x; V y; } u;
  u.y = x;
  return u.x + 1;
}
#endif

#if __BITINT_MAXWIDTH__ >= 512
W
f21 (_BitInt(512) x)
{
  union U { _BitInt(512) x; W y; } u;
  u.x = x;
  return u.y;
}

W
f22 (_BitInt(509) x)
{
  union U { _BitInt(509) x; W y; } u;
  u.x = x;
  return u.y;
}

_BitInt(512)
f23 (W x)
{
  union U { _BitInt(512) x; W y; } u;
  u.y = x;
  return u.x;
}

_BitInt(506)
f24 (W x)
{
  union U { _BitInt(506) x; W y; } u;
  u.y = x;
  return u.x;
}

W
f25 (_BitInt(512) x)
{
  union U { _BitInt(512) x; W y; } u;
  u.x = x + 1;
  return u.y;
}

W
f26 (_BitInt(509) x)
{
  union U { _BitInt(509) x; W y; } u;
  u.x = x + 1;
  return u.y;
}

W
f27 (_BitInt(512) *x)
{
  union U { _BitInt(512) x; W y; } u;
  u.x = *x + 1;
  return u.y;
}

W
f28 (_BitInt(509) *x)
{
  union U { _BitInt(509) x; W y; } u;
  u.x = *x + 1;
  return u.y;
}

_BitInt(512)
f29 (W x)
{
  union U { _BitInt(512) x; W y; } u;
  u.y = x;
  return u.x + 1;
}

_BitInt(506)
f30 (W x)
{
  union U { _BitInt(506) x; W y; } u;
  u.y = x;
  return u.x + 1;
}
#endif
