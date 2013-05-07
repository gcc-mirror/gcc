/* PR tree-optimization/57149 */
/* { dg-do compile } */
/* { dg-options "-Os -Wuninitialized" } */

struct A { struct A *a, *b; };
struct D { struct A e; };
struct E { unsigned char f; struct { struct A e; } g; };
struct F { struct E i[32]; };

extern int fn0 (void);
extern int fn1 (struct E *, struct D *);

static inline __attribute__ ((always_inline)) int
fn2 (const struct A *x)
{
  return x->a == x;
}

static int
fn3 (struct E *x)
{
  struct D *l, *m;
  int retval = retval;
  if (fn2 (&x->g.e))
    return 0;
  for (l = (struct D *) x->g.e.a, m = (struct D *) l->e.a;
       &l->e != &x->g.e;
       l = m, m = (struct D *) m->e.a)
    retval = fn1 (x, l);
  return retval;
}

void
fn4 (struct F *x, unsigned k)
{
  unsigned i;
  for (i = 0; i < k; i++)
    {
      struct E *y = &x->i[i];
      int err = -22;
      err = fn3 (y);
      if (y->f == 0)
	{
	  if (err > 0)
	    err = fn0 ();
	  if (err < 0)	/* { dg-bogus "may be used uninitialized in this function" } */
	    fn0 ();
	}
    }
}
