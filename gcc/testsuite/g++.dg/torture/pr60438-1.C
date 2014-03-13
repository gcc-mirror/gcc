// { dg-do compile }
// { dg-options "-fomit-frame-pointer" }

struct A { int a; };
struct B { A foo (); };
struct C { B *foo (); };
int foo (struct C *, float);
void bar (struct C *);
void baz (struct A *);
int a, b, c;

int
foo (struct C *y, float x)
{
  struct A d;
  if (c)
    bar (y);
  else
    {
      C g;
      g.foo ()->foo ();
      a = b;
      d.a = (int) (b * x);
    }
  baz (&d);
}
