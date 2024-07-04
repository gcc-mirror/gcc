// PR c++/85889
// { dg-do compile { target c++17 } }
// { dg-options "" }

struct X { int i, j; };
void f() {
  X x{};
  auto [i, j] = x;
  [&i]() { }; // { dg-warning "captured structured bindings" "" { target c++17_only } }
  [i]() { }; // { dg-warning "captured structured bindings" "" { target c++17_only } }
}

