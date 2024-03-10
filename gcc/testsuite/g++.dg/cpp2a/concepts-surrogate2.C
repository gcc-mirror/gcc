// PR c++/110535
// { dg-do compile { target c++20 } }

using F = int(int);
using G = long(int);

template<bool B>
struct A {
 operator F&() requires B;
 operator G&() requires (!B);
};

int i = A<true>{}(0);  // { dg-bogus "ambiguous" }
int j = A<false>{}(0); // { dg-bogus "ambiguous" }
