/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a -mautovec-preference=asimd-only -fno-schedule-insns -fno-reorder-blocks -fno-schedule-insns2 -fdump-tree-vect-details" }*/
/* { dg-final { check-function-bodies "**" "" } } */

char p[128];

/*
** fand:
**	...
** 	uminp	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
** 	fmov	x[0-9]+, d[0-9]+
** 	cmn	x[0-9]+, #1
** 	cset	w[0-9]+, eq
**	...
*/
bool __attribute__((noipa))
fand (int n)
{
  bool r = true;
  for (int i = 0; i < n; ++i)
    r &= (p[i] != 0);
  return r;
}

/*
** fior:
**	...
** 	umaxp	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
** 	fmov	x[0-9]+, d[0-9]+
** 	cmp	x[0-9]+, 0
** 	cset	w[0-9]+, ne
**	...
*/
bool __attribute__((noipa))
fior (int n)
{
  bool r = false;
  for (int i = 0; i < n; ++i)
    r |= (p[i] != 0);
  return r;
}

/* 
** fxor:
**	...
** 	movi	v[0-9]+.16b, 0x1
** 	and	v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b
** 	addv	b[0-9]+, v[0-9]+.16b
** 	fmov	w[0-9]+, s[0-9]+
** 	and	w[0-9]+, w[0-9]+, 1
**	...
*/
bool __attribute__((noipa))
fxor (int n)
{
  bool r = false;
  for (int i = 0; i < n; ++i)
    r ^= (p[i] != 0);
  return r;
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 3 "vect" } } */

