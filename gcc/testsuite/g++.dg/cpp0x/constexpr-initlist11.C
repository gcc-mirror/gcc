// PR c++/89144
// { dg-do link { target c++11 } }

#include <initializer_list>

template <class> void b() {
  static constexpr std::initializer_list<int> c{ 42 };
  constexpr std::initializer_list<int> l { };
}

int main() { b<int>(); }
