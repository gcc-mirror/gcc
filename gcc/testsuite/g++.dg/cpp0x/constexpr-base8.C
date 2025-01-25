// PR c++/118239
// { dg-do "compile" { target c++11 } }

struct NoMut1 {
  int a, b;
};

// Reported case.
struct NoMut2 : NoMut1 {
  constexpr NoMut2(int a, int b) /*: NoMut1()*/
  {} // { dg-error "must be initialized" "" { target c++17_down } }
};

// Variant with explicit initialization of some member.
struct NoMut3 : NoMut1 {
  constexpr NoMut3(int a, int b) : c(0) /*, NoMut1()*/
  {} // { dg-error "must be initialized" "" { target c++17_down } }
  int c;
};

void mutable_subobjects() {
  constexpr NoMut2 nm2(1, 2); // { dg-error "constant expression" }
  constexpr NoMut3 nm3(1, 2); // { dg-error "constant expression" }
}
