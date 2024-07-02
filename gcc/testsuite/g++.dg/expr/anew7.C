// PR c++/115645
// { dg-do compile { target c++11 } }

struct S {
  explicit S() { }
};

auto p = new S[1][1]();
auto q = new S[1][1]{}; // { dg-error "explicit" }
auto r = new S[1]();
auto s = new S[1]{}; // { dg-error "explicit" }
auto t = new S[1][1][1]();
auto u = new S[1][1][1]{}; // { dg-error "explicit" }
