/* PR target/51957 */
/* { dg-do link } */
/* { dg-options "-O2 -g -fprofile-use -Wno-missing-profile" } */
/* { dg-additional-sources "pr51957-2.c" } */

int v[128];
#include "pr51957-1.h"

void
foo (U *x)
{
  T *a = x->u;
  while (1)
    {
      union R *b;
      b = fn1 ();
      if (b != w[0] && !(v[b->p->c] == 1))
	{
	  fn2 (a->t, "foobar", b->p);
	  b = w[0];
	}
      if (b != w[0])
	fn3 ();
      if (w[0] && b != w[0])
	fn4 (b->p);
      if (b != w[0] && (v[b->p->c] == 1) && fn4 (b->p))
	break;
    }
}
