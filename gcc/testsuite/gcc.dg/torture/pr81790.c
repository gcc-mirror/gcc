/* { dg-do compile } */

typedef int a __attribute__ ((__vector_size__ (16)));
typedef struct
{
  a b;
} c;

int d, e;

void foo (c *ptr);

void bar ()
{
  double b = 1842.9028;
  c g, h;
  if (d)
    b = 77.7998;
  for (; e;)
    {
      g.b = g.b = g.b + g.b;
      h.b = (a){b};
      h.b = h.b + h.b;
    }
  foo (&g);
  foo (&h);
}
