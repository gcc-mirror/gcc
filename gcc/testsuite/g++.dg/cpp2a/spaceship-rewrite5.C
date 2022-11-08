// { dg-do compile { target c++11 } }

template<typename T>
struct A {
  A() {}

  template<typename U>
    A(const A<U>&) {}

  bool operator==(const A&) const { return true; }
};

A<const int> a;
A<int> b;
auto c = (a == b); // { dg-error "ambiguous, even though the second is reversed" "" { target c++20 } }
