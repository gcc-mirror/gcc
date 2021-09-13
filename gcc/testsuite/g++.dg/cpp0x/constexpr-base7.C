// PR c++/91933
// { dg-do compile { target c++11 } }

struct NoMut1 { int a, b; };
struct NoMut3 : NoMut1 {
  constexpr NoMut3(int a, int b) : NoMut1{a, b} {}
};
void mutable_subobjects() {
  constexpr NoMut3 nm3 = {1, 2};
  struct A {
    void f() {
      static_assert(nm3.a == 1, "");
    }
  };
}
