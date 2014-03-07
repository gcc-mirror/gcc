// Core 1321
// { dg-do compile { target c++11 } }
// Two dependent names are equivalent even if the overload sets found by
// phase 1 lookup are different.  Merging them keeps the earlier set.

int g1(int);
template <class T> decltype(g1(T())) f1();
int g1();
template <class T> decltype(g1(T())) f1()
{ return g1(T()); }
int i1 = f1<int>();	    // OK, g1(int) was declared before the first f1

template <class T> decltype(g2(T())) f2(); // { dg-error "g2. was not declared" }
int g2(int);
template <class T> decltype(g2(T())) f2()
{ return g2(T()); }
int i2 = f2<int>();			  // { dg-error "no match" }

int g3();
template <class T> decltype(g3(T())) f3(); // { dg-error "too many arguments" }
int g3(int);
template <class T> decltype(g3(T())) f3()
{ return g3(T()); }
int i3 = f3<int>();			  // { dg-error "no match" }
