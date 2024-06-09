// PR c++/112588

void f(int*);

template <typename T>
struct S {
  void g(int n) { f(&n); }
};

template struct S<void>;
