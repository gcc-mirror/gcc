// PR c++/118225
// { dg-do "compile" { target c++11} }

struct NoMut1 { int a, b; };
struct NoMut3 : virtual NoMut1 {
  constexpr NoMut3(int a, int b)
    : NoMut1{a, b}
  {} // { dg-error "virtual base" "" { target c++23_down } }
};
void mutable_subobjects() {
  constexpr NoMut3 nm3 = {1, 2}; // { dg-error "call to non" "" { target c++23_down } }
  struct A {
    void f() {
      static_assert(nm3.a == 1, ""); // { dg-error "local variable" "" { target c++23_down } }
    }
  };
}
