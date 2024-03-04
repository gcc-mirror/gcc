// PR c++/107688
// P2615R1: Meaningful exports: Newly invalid declarations
// { dg-do compile { target c++11 } }

extern "C++" template <typename T> struct A {};

extern "C++" template <typename T> struct A<T*> {};

extern "C++" template <> struct A<int*> {};
// { dg-error "explicit specializations are not permitted here" "" { target c++20 } .-1 }

extern "C++" template struct A<int>;
// { dg-error "explicit instantiations are not permitted here" "" { target c++20 } .-1 }


// These should all still be valid, though
extern "C++" {
  template <typename T> struct B {};
  template <typename T> struct B<T*> {};
  template <> struct B<int*> {};
  template struct B<int>;
}
