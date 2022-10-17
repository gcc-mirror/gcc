// PR c++/102050
// { dg-do compile { target c++11 } }

#include <initializer_list>

extern struct A a;

struct A {
  A(const A& = a);
  A(std::initializer_list<int>) = delete;
};

void f(A);

int main() {
  f({}); // { dg-bogus "deleted" }
}
