// PR c++/114954
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi main }

export module main;

template <int N>
union U {
private:
  char a[N + 1];
  int b;
};

U<4> p;
