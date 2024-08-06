/* { dg-do run } */
/* { dg-additional-options "-O3 -std=c99 -save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

#pragma GCC target "+nosve"

#include <string.h>

typedef int v4si __attribute__ ((vector_size (16)));


/*
**foo1:
**	cmgt	v0.4s, v1.4s, v0.4s
**	bsl	v0.16b, v2.16b, v3.16b
**	ret
*/
v4si foo1 (v4si a, v4si b, v4si c, v4si d) {
    return ((a < b) & c) | ((a >= b) & d);
}

/*
**foo2:
**	cmgt	v0.4s, v1.4s, v0.4s
**	bsl	v0.16b, v3.16b, v2.16b
**	ret
*/
v4si foo2 (v4si a, v4si b, v4si c, v4si d) {
    return (~(a < b) & c) | (~(a >= b) & d);
}


/* The bsl could be bit or bif depending on register
   allocator inside the loop. */
/**
**bar1:
**...
**	cmge	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	(bsl|bit|bif)	v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b
**	and	v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b
**...
*/
void bar1 (int * restrict a, int * restrict b, int * restrict c,
	  int * restrict d, int * restrict res, int n)
{
  for (int i = 0; i < (n & -4); i++)
    res[i] = ((a[i] < b[i]) & c[i]) | ((a[i] >= b[i]) & d[i]);
}

/* The bsl could be bit or bif depending on register
   allocator inside the loop. */
/**
**bar2:
**...
**	cmge	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
**	(bsl|bit|bif)	v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b
**...
*/
void bar2 (int * restrict a, int * restrict b, int * restrict c,
	  int * restrict d, int * restrict res, int n)
{
  for (int i = 0; i < (n & -4); i++)
    res[i] = (-(a[i] < b[i]) & c[i]) | (-(a[i] >= b[i]) & d[i]);
}

extern void abort ();

int main ()
{

  v4si a = { -3, -3, -3, -3 };
  v4si b = { 3, 3, 3, 3 };
  v4si c = { 5, 5, 5, 5 };
  v4si d = { 8, 8, 8, 8 };

  v4si res1 = foo1 (a, b, c, d);
  if (memcmp (&res1, &c, 16UL) != 0)
    abort ();

  v4si res2 = foo2 (a, b, c, d);
  if (memcmp (&res2, &d, 16UL) != 0)
   abort ();

  int ar[4] = { -3, -3, -3, -3 };
  int br[4] = { 3, 3, 3, 3 };
  int cr[4] = { 5, 5, 5, 5 };
  int dr[4] = { 8, 8, 8, 8 };

  int exp1[4] = { 1, 1, 1, 1 };
  int res3[4];
  bar1 ((int*)&ar, (int*)&br, (int*)&cr, (int*)&dr, (int*)&res3, 4);
  if (memcmp (&res3, &exp1, 16UL) != 0)
    abort ();

  int res4[4];
  bar2 ((int*)&ar, (int*)&br, (int*)&cr, (int*)&dr, (int*)&res4, 4);
  if (memcmp (&res4, &cr, 16UL) != 0)
    abort ();

  return 0;
}
