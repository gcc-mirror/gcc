// PR tree-optimization/51596
// { dg-do compile }
// { dg-options "-O -fnon-call-exceptions" }

struct A { float v[2]; };
struct B { int v[2]; };

struct C
{
  B c;
  C f ()
  {
    B b;
    for (int i = 0; i < 2; i++)
      b.v[i] = c.v[i];
    return *this;
  }
};

struct D
{
  A d;
  D (B x)
  {
    for (int i = 0; i < 2; i++)
      d.v[i] = x.v[i];
  }
};

int bar ();

C i;

void
foo ()
{
  while (bar ())
    D (i.f ().c);
}
