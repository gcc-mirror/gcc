// PR c++/115062
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename>
struct S {
  int a;
  long b;
  union {};
  ~S();
  void foo();
};
extern template void S<char>::foo();
S<char> operator+(S<char>, const char *);
