// PR c++/93530 - ICE on invalid alignas in a template.
// { dg-do compile { target c++11 } }

template <typename T> struct S {
  using U = S;
  void fn() alignas(U::X); // { dg-error "not a member" }
};
