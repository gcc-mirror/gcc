// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S {
  template <class U>
  friend struct S2;
};

template struct S<int>;
