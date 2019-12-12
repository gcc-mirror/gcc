/* { dg-do compile } */
/* { dg-options "-O -ftree-slp-vectorize" } */

char *bar (void);
__INTPTR_TYPE__ baz (void);

void
foo (__INTPTR_TYPE__ *q)
{
  char *p = bar ();
  __INTPTR_TYPE__ a = baz ();
  __INTPTR_TYPE__ b = baz ();
  int i = 0;
#define X q[i++] = a; q[i++] = b; a = a + b; b = b + a;
#define Y X X X X X X X X X X
#define Z Y Y Y Y Y Y Y Y Y Y
  Z Z Z Z Z Z Z Z Z Z
      p[a] = 1;
  p[b] = 2;
}
