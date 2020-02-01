// PR c++/93530 - ICE on invalid alignas in a template.
// { dg-do compile { target c++11 } }

template <typename T> struct S {
  using U = S;
  // FIXME: This is ill-formed; see PR90847.
  void fn() alignas(U::X);
};
