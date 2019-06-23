/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

typedef __INTPTR_TYPE__ intptr_t;
intptr_t a, b, c, d;
int foo (void) { return 0; }
int baz (void);

void
bar (void)
{
  intptr_t g = (intptr_t) &&h;
  void *i = &&j, *k = &&l;
j:
  if (baz ())
    {
      intptr_t **n = (intptr_t **) &a;
l:
      b = 0;
      for (; b >= 0;)
	goto *k;
h:
      **n = 0;
      for (;;)
	{
	  intptr_t *o = &c;
	  g = foo ();
	  *o = g;
	  if (c)
	    goto *d;
	}
    }
  goto *i;
}
