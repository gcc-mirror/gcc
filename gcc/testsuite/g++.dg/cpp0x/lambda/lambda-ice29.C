// PR c++/84618
// { dg-do compile { target c++11 } }

template <int>
struct S {
  void b() const;
  void b() { [b] {}; } // { dg-error "15:capture of non-variable" }
};
