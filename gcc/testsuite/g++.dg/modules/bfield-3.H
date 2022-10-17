// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<int N>
struct A {
  int x : 1;
  int y : N;
};
