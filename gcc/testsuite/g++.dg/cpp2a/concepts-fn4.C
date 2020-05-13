// Testcase from [expr.prim.id]/5
// { dg-do compile { target c++20 } }

template<typename T> struct A {
  static void f(int) requires false;
};
void g() {
  A<int>::f(0);			// { dg-error "" "cannot call f" }
  void (*p1)(int) = A<int>::f;  // { dg-error "" "cannot take the address of f" }
  decltype(A<int>::f)* p2 = nullptr; // { dg-error "" "the type decltype(A<int>::f) is invalid" }
}
