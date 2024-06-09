// PR c++/115114
// { dg-do compile { target c++20 } }

struct X {} x;
struct Y {} y;

template<class T, class U>
struct A : T {
  U m;
};

using ty1 = decltype(A{x, 42}); // OK
using ty1 = decltype(A(x, 42)); // OK, used to fail
using ty1 = A<X, int>;

template<class T, class U = int, class V = int>
struct B : T, V {
  U m = 42;
};

using ty2 = decltype(B{x, y}); // OK
using ty2 = decltype(B(x, y)); // OK, used to fail
using ty2 = B<X, int, Y>;
