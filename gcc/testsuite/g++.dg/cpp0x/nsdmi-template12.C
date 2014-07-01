// PR c++/58753
// { dg-do compile { target c++11 } }

#include <initializer_list>

template <class T>
struct X {X(std::initializer_list<int>) {}};

template <class zomg>
class T {
  X<T> x{1};
};

int main()
{
  T<int> t;
}
