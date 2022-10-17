// PR c++/103049
// P0849R8 - auto(x)
// { dg-do compile { target c++23 } }
// Testcase from P0849R8.

struct A {};
void f(A&) = delete;  // #1
void f(A&&); // #2
A& g();
void h() {
//  f(g());      // calls #1
  f(A(g()));     // calls #2 with a temporary object
  f(auto(g()));  // calls #2 with a temporary object
}
