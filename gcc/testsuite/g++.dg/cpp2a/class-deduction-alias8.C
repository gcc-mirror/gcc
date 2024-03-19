// PR c++/95486
// { dg-do compile { target c++20 } }

template <class T>
concept Int = __is_same (T, int);

template<class T, class U>
struct X { X(U) requires Int<U> {} };

template<class U>
X(U) -> X<char, U>;

template<class U>
using Y = X<void, U>;

Y y{1};
Y z{'a'}; // { dg-error "failed|no match" }
