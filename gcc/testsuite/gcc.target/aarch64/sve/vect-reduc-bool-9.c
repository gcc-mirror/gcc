/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a+sve -mautovec-preference=sve-only -fno-schedule-insns -fno-reorder-blocks -fno-schedule-insns2 -fdump-tree-vect-details" }*/
/* { dg-final { check-function-bodies "**" "" } } */

char p[128];

/*
** fand:
** 	...
** 	ptrue	p[0-9]+.s, all
** 	nots	p[0-9]+.b, p[0-9]+/z, p[0-9]+.b
** 	cset	w[0-9]+, none
** 	and	w[0-9]+, w[0-9]+, w[0-9]+
** 	...
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
** 	...
** 	ptest	p[0-9]+, p[0-9]+.b
** 	cset	w[0-9]+, any
** 	orr	w[0-9]+, w[0-9]+, w[0-9]+
** 	...
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
** 	...
** 	cntp	x[0-9]+, p[0-9]+, p[0-9]+.h
** 	and	w[0-9]+, w[0-9]+, 1
** 	eor	w[0-9]+, w[0-9]+, w[0-9]+
** 	...
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
