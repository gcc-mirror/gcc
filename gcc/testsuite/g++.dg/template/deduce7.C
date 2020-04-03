// PR c++/90505 - mismatch in template argument deduction.
// { dg-do compile { target c++11 } }

template <typename> class a {
  using b = int;
  using c = int;
  b d;
  void e() { g<c>(d); }
  template <typename... f> static void g(f...);
};
