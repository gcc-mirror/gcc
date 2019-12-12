// PR c++/89144
// { dg-do link { target c++11 } }

#include <initializer_list>

template <class> void b() {
  constexpr std::initializer_list<int> l { 42 }; // { dg-error "" }
}

int main() { b<int>(); }
