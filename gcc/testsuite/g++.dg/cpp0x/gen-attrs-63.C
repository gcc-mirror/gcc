// PR c++/79967
// { dg-do compile { target c++11 } }

template <void f [[noreturn]]()>
struct A
{
  int g () { f (); return 0; }
};

void f ();

void g (A<f> a) { a.g (); }
