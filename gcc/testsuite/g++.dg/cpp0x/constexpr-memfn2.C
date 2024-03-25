// PR c++/102420
// { dg-do compile { target c++11 } }

struct X {
  constexpr int f() { return 0; }
};
constexpr int g(X* x) {
  return x->f();  // { dg-error "dereferencing a null pointer" }
}
constexpr int t = g(nullptr);  // { dg-message "in .constexpr. expansion" }
