// PR c++/110535
// { dg-do compile { target c++20 } }

using F = int(int);

template<bool B>
struct A {
 operator F*() requires B;
};

int i = A<true>{}(0);  // OK
int j = A<false>{}(0); // { dg-error "no match" }
