// PR c++/95963
// { dg-do compile }
// { dg-options "-Wnonnull" }

struct A { virtual void foo (); };

void
bar (A *p)
{
  __builtin_launder (p)->foo ();
}
