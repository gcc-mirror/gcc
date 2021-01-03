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
  X<int> c;			// { dg-error "no match" }

  auto operator <=>(Y const&) const = default;
};

void f()
{
  auto x = Y() < Y();		// { dg-error "deleted" }
}
