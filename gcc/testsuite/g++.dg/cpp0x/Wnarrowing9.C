// PR c++/57891
// { dg-do compile { target c++11 } }

// N is value-dependent, don't warn.
template<int N> struct S { char a[N]; }; // { dg-bogus "narrowing conversion" }
S<1> s;
