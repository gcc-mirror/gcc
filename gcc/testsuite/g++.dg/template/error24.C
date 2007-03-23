// PR c++/30863

template <typename T>
struct s {};

void f() {
  unsigned s<int> x; // { dg-error "invalid" }
}
