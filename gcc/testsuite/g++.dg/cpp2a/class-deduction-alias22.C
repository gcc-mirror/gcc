// PR c++/115198
// { dg-do compile { target c++20 } }

template<bool B, class T>
struct C {
  C() = default;
  C(const C&) = default;
};

template<class T>
using A = C<false, T>;

C<false, int> c;
A a = c; // { dg-bogus "ambiguous" }
