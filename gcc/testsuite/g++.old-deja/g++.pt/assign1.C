// { dg-do compile  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S { // { dg-error "assignment" }
  S();
  T t;
};

void f()
{
  S<const int> s;
  s = s; // { dg-error "synthesized" }
}
