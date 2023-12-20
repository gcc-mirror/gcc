/* PR tree-optimization/112941 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 4096
void
f1 (_BitInt(4096) *p, int r, _BitInt(115) s, _BitInt(128) t, _BitInt(231) u)
{
  p[0] += (unsigned _BitInt(2048)) r;
  p[1] += (unsigned _BitInt(2048)) s;
  p[2] += (unsigned _BitInt(2048)) t;
  p[3] += (unsigned _BitInt(2048)) u;
}

void
f2 (_BitInt(4094) *p, int r, _BitInt(115) s, _BitInt(128) t, _BitInt(231) u)
{
  p[0] -= (unsigned _BitInt(2048)) r;
  p[1] -= (unsigned _BitInt(2048)) s;
  p[2] -= (unsigned _BitInt(2048)) t;
  p[3] -= (unsigned _BitInt(2048)) u;
}

void
f3 (_BitInt(4096) *p, int r, _BitInt(115) s, _BitInt(128) t, _BitInt(231) u)
{
  p[0] += (unsigned _BitInt(2110)) r;
  p[1] += (unsigned _BitInt(2110)) s;
  p[2] += (unsigned _BitInt(2110)) t;
  p[3] += (unsigned _BitInt(2110)) u;
}

void
f4 (_BitInt(4094) *p, int r, _BitInt(115) s, _BitInt(128) t, _BitInt(231) u)
{
  p[0] -= (unsigned _BitInt(2110)) r;
  p[1] -= (unsigned _BitInt(2110)) s;
  p[2] -= (unsigned _BitInt(2110)) t;
  p[3] -= (unsigned _BitInt(2110)) u;
}

void
f5 (unsigned _BitInt(4096) *p, int r, _BitInt(115) s, _BitInt(128) t, _BitInt(231) u)
{
  p[0] += (unsigned _BitInt(2048)) r;
  p[1] += (unsigned _BitInt(2048)) s;
  p[2] += (unsigned _BitInt(2048)) t;
  p[3] += (unsigned _BitInt(2048)) u;
}

void
f6 (unsigned _BitInt(4094) *p, int r, _BitInt(115) s, _BitInt(128) t, _BitInt(231) u)
{
  p[0] -= (unsigned _BitInt(2048)) r;
  p[1] -= (unsigned _BitInt(2048)) s;
  p[2] -= (unsigned _BitInt(2048)) t;
  p[3] -= (unsigned _BitInt(2048)) u;
}

void
f7 (unsigned _BitInt(4096) *p, int r, _BitInt(115) s, _BitInt(128) t, _BitInt(231) u)
{
  p[0] += (unsigned _BitInt(2110)) r;
  p[1] += (unsigned _BitInt(2110)) s;
  p[2] += (unsigned _BitInt(2110)) t;
  p[3] += (unsigned _BitInt(2110)) u;
}

void
f8 (unsigned _BitInt(4094) *p, int r, _BitInt(115) s, _BitInt(128) t, _BitInt(231) u)
{
  p[0] -= (unsigned _BitInt(2110)) r;
  p[1] -= (unsigned _BitInt(2110)) s;
  p[2] -= (unsigned _BitInt(2110)) t;
  p[3] -= (unsigned _BitInt(2110)) u;
}

#if __SIZEOF_INT128__
void
f9 (_BitInt(4096) *p, __int128 r)
{
  p[0] += (unsigned _BitInt(2048)) r;
}

void
f10 (_BitInt(4094) *p, __int128 r)
{
  p[0] -= (unsigned _BitInt(2048)) r;
}

void
f11 (_BitInt(4096) *p, __int128 r)
{
  p[0] += (unsigned _BitInt(2110)) r;
}

void
f12 (_BitInt(4094) *p, __int128 r)
{
  p[0] -= (unsigned _BitInt(2110)) r;
}

void
f13 (unsigned _BitInt(4096) *p, __int128 r)
{
  p[0] += (unsigned _BitInt(2048)) r;
}

void
f14 (unsigned _BitInt(4094) *p, __int128 r)
{
  p[0] -= (unsigned _BitInt(2048)) r;
}

void
f15 (unsigned _BitInt(4096) *p, __int128 r)
{
  p[0] += (unsigned _BitInt(2110)) r;
}

void
f16 (unsigned _BitInt(4094) *p, __int128 r)
{
  p[0] -= (unsigned _BitInt(2110)) r;
}
#endif
#else
int i;
#endif
