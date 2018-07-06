// PR c++/84596
// { dg-do compile { target c++11 } }

template<int x>
void b(int c) {
  static_assert (c, "c"); // { dg-error "non-constant|not a constant" }
}
