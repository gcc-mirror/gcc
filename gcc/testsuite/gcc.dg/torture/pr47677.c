/* { dg-do compile } */

struct S { int s; int u; };
extern int bar (void);
extern struct S *baz (void);

void
foo (int x, struct S *y, char z, int *v, struct S *s)
{
  int c, d;
  if (x & 2)
    d = 1;
  else
    {
      d = (x & 1) ? 11 : 0;
      while ((c = (s->s != (s->u & 1) ? s->s : bar ())) != '\0');
      c = (s->s != '\\' && (s->u & 4) ? s->s : bar ());
      if (c == '<')
        goto lab;
    }
  while ((c = ((s->u & 1) ? s->s : bar ())) != 0
         && ((d != 11 && d != 17) || (v[c] & 1) == 0))
    {
    lab:;
      switch (d)
        {
        case 14:
          if (c == '}')
            y = baz ();
          d = y->s = z == '<' ? 17 : 11;
        }
    }
}

