// { dg-do assemble  }
// Origin: Neil Booth, from bug report #44

#include <iterator>

template<class T>
struct X
{
};

template<class T>
X<T> operator+(const X<T>&, const X<T>&);

template<>
X<int> operator+<int>(const X<int>&, const X<int>&);
