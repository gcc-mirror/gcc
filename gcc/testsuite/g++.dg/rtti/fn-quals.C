// PR c++/48665

#include <typeinfo>
extern "C" void abort();

template<class A,class B> void f() {
  if (typeid(A)==typeid(B)) abort(); // { dg-error "qualified function" }
  if (typeid(A*)==typeid(B*)) abort(); // { dg-error "qualified function" }
}
int main() {
  f<void()const,void()>();
}
