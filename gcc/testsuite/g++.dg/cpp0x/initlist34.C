// PR c++/44157
// { dg-options "-std=c++0x" }

#include <initializer_list>

template<typename T>
void f(T) { }

int main() {
  std::initializer_list<int> a = { 0 };
  f(a);

  f<std::initializer_list<int> >({ 0 });
}
