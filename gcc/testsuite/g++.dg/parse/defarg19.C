// PR c++/117324
// { dg-do "compile" { target c++11 } }

template<typename = int>
struct ct1 {
  friend void f(ct1, // { dg-error "specifies default" }
		int = [](int p = [] {}) { return p; }(); ) // { dg-error "expected|declares a non-template" }
  {}
};
void test() {
  f(ct1{}); // { dg-error "missing template arguments|not declared" "" { target c++14_down } }
}
