// PR c++/104823
// { dg-do compile { target c++11 } }

struct S { S(int); };
double id(double);
template<class> auto f(double v) -> decltype(S{id(v)}); // { dg-error "narrowing" }
