// PRMS Id: 4864
// Bug: g++ can't deal with a guiding declaration which comes before the
// template.
// Build don't link:

void f (const int&, const int&);
template <class T> void f (const T&, const T&) { }

void g (int a)
{
  f (a,a); // gets bogus error - two identical candidates
}
