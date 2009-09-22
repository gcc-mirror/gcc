/* { dg-do compile } */
/* { dg-options "-O2 -g -funroll-loops -std=gnu99" } */

struct S
{
  unsigned long s1;
  int **s2;
};
struct T
{
  unsigned long t1, t2;
};
struct U
{
  int u1, u2;
  unsigned long u3;
};
struct V
{
  int v1, v3;
  struct T *v2;
  struct U *v4;
};
struct W
{
  int w1;
  struct V **w2;
};
struct S *foo1 (void);
int *foo2 (void);

void
test (struct W *w)
{
  for (int i = 0; i < w->w1; i++)
    {
      struct V *v = w->w2[i];
      struct S *t = foo1 ();
      if (!t)
	for (int j; j < v->v1;)
	  {
	    struct T *q = &v->v2[j];
	    t += (q->t2 - q->t1) * 45000L;
	  }
      for (; v->v3;)
	{
	  struct U *v4 = (struct U *) &v->v4;
	  if (v4->u1 && v4->u2 >= 0 && v4->u2)
	    {
	      int *s = foo2 ();
	      if (!s)
		for (int k = 0; k <= v4->u2; k++)
		  {
		    struct T *q = &v->v2[k];
		    if (k == v4->u2)
		      v4->u3 += (q->t1) * 1000000;
		  }
	      t->s2[t->s1] = s;
	    }
	}
      int *s = foo2 ();
      if (!t)
	t->s2[t->s1] = s;
    }
}
