// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {};

struct X {};

void f () {
  typedef X Y;
  S<Y> s;
}
