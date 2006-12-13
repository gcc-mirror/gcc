// This testcase used to hang the compiler in vt_find_locations.
// { dg-do compile }
// { dg-options "-O2 -g" }

struct S
{
  int a;
  S *b, *c, *d;
};

struct T
{
  void f1 (S *x);
  void f2 (S *x);
  void f3 (S *x, S *y);
  S *e;
};

void
T::f3 (S *x, S *y)
{
  while (x != this->e && (!x || x->a == 1))
    {
      if (x == y->c)
	{
	  S *w = y->d;
	  if (w && w->a == 0)
	    {
	      w->a = 1;
	      y->a = 0;
	      f2 (y);
	      w = y->d;
	    }
	  if (w && (!w->c || w->c->a == 1) && (!w->d || w->d->a == 1))
	    {
	      w->a = 0;
	      x = y;
	      y = x->b;
	    }
	  else
	    {
	      if (w && (!w->d || w->d->a == 1))
		{
		  if (w->c)
		    w->c->a = 1;
		  w->a = 0;
		  f1 (w);
		  w = y->d;
		}
	      if (w)
		{
		  w->a = y->a;
		  if (w->d)
		    w->d->a = 1;
		}
	      y->a = 1;
	      f2 (y);
	      x = e;
	    }
	}
      else
	{
	  S *w = y->c;
	  if (w && w->a == 0)
	    {
	      w->a = 1;
	      y->a = 0;
	      f1 (y);
	      w = y->c;
	    }
	  if (w && (!w->c || w->c->a == 1) && (!w->d || w->d->a == 1))
	    {
	      w->a = 0;
	      x = y;
	      y = x->b;
	    }
	  else
	    {
	      if (w && (!w->c || w->c->a == 1))
		{
		  w->a = 0;
		  if (w->d)
		    w->d->a = 1;
		  f2 (w);
		  w = y->c;
		}
	      if (w)
		{
		  w->a = y->a;
		  if (w->c)
		    w->c->a = 1;
		}
	      y->a = 1;
	      f1 (y);
	      x = e;
	    }
	}
    }
}
