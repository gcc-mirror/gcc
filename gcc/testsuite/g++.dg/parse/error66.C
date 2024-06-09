// PR c++/105760
// { dg-do compile { target c++17 } }

template<class... Ts, class> // { dg-error "must be at the end of the template parameter list" }
struct A { A(Ts...); };
A a;
