// PR c++/44157
// { dg-do compile { target c++11 } }

#include <initializer_list>

template<typename T>
void f(T) { }

int main() {
  std::initializer_list<int> a = { 0 };
  f(a);

  f<std::initializer_list<int> >({ 0 });
}
