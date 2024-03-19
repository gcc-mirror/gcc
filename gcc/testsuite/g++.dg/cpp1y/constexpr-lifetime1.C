// PR c++/96630
// { dg-do compile { target c++14 } }

struct S {
  int x = 0;
  constexpr const int& get() const { return x; }
};

constexpr const int& test() {
  auto local = S{};  // { dg-message "note: declared here" }
  return local.get();
}
constexpr int x = test();  // { dg-error "accessing .local. outside its lifetime" }
