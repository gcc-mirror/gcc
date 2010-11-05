// PR c++/46160
// { dg-do compile }

struct S
{
  enum E { A };
} s;
volatile S t;

void f (S::E);

void
g ()
{
  volatile S *p = &s;
  f (p->A);
  f (t.A);
}
