// PR c++/114950

template <typename T>
struct A {
  friend void x();
};
template <typename T>
struct B {
  virtual void f() { A<T> r; }
};
template struct B<int>;
