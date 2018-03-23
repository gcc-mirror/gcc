// PR c++/84445
// { dg-do compile }

struct A { virtual void foo (); };

void
bar (A *p)
{
  __builtin_launder (p)->foo ();
}
