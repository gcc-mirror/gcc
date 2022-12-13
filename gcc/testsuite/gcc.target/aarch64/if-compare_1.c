/* { dg-do run } */
/* { dg-additional-options "-O -save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

extern void abort ();

/*
**zoo1:
**	cmp	w0, w1
**	csel	w0, w2, w3, lt
**	and	w0, w0, 1
**	ret
*/
__attribute((noipa, noinline))
int zoo1 (int a, int b, int c, int d)
{
   return ((a < b) & c) | ((a >= b) & d);
}

/*
**zoo2:
**	cmp	w0, w1
**	csel	w0, w2, w3, lt
**	ret
*/
__attribute((noipa, noinline))
int zoo2 (int a, int b, int c, int d)
{
   return (-(a < b) & c) | (-(a >= b) & d);
}

int main ()
{
  if (zoo1 (-3, 3, 5, 8) != 1)
    abort ();

  if (zoo1 (3, -3, 5, 8) != 0)
    abort ();

  if (zoo2 (-3, 3, 5, 8) != 5)
    abort ();

  if (zoo2 (3, -3, 5, 8) != 8)
    abort ();

  return 0;
}
