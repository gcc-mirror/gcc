// PR c++/31027

struct A {};

template<typename T>
struct C: virtual A {
  C() {}
  template<typename T_OTHER> C(const C<T_OTHER>&) {}
  C func(const class C<long>&) const;
  operator bool() const;
};

template<typename T>
struct D: C<T> {
  void func2() {
    C<int>a;
    a.func(a);
  }
};

void func3() {
  C<int>a;
  a.func(a);
}
