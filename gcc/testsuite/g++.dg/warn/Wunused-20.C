// PR c++/114104
// { dg-additional-options "-Wunused-value" }

bool f();
struct A { int i; };
A& g();

void h() {
  !f(); // { dg-warning "value computed is not used" }
  &g().i; // { dg-warning "value computed is not used" }
}

#if  __cplusplus >= 202002L
[[nodiscard]] bool operator==(A&, int);

void h(A a) {
  a != 0; // { dg-warning "value computed is not used" "" { target c++20 } }
}
#endif
