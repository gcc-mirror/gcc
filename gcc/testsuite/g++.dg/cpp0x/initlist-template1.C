// PR c++/70824
// { dg-do compile { target c++11 } }

#include <initializer_list>

constexpr
int
max(std::initializer_list<int> __l)
{ return *__l.begin(); }

template <class Src>
void
a() {
  const int v =  max({1});
}
