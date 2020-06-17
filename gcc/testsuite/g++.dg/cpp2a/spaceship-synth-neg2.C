// PR c++/92774
// { dg-do compile { target c++20 } }

#include <compare>

template<typename T>
struct X { };

template<typename T>
bool operator==(const X<T>&, const X<T>&) { return true; }
template<typename T>
bool operator<(const X<T>&, const X<T>&) { return true; }

struct Y
{
  int a;
  X<int> c;

  auto operator <=>(Y const&) const = default; // { dg-error "no match" }
};

void f()
{
  auto x = Y() < Y();		// { dg-error "deleted" }
}
