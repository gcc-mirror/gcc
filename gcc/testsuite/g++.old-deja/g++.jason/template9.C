// { dg-do assemble  }
// PRMS Id: 4864
// Bug: g++ can't deal with a guiding declaration which comes before the
// template.

void f (const int&, const int&);
template <class T> void f (const T&, const T&) { }

void g (int a)
{
  f (a,a); // { dg-bogus "" } two identical candidates
}
