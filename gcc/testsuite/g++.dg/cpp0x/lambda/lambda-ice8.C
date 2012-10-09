// PR c++/51422
// { dg-do compile { target c++11 } }

template<typename> struct A {};

void foo()
{
  [i] { A<decltype(i)>(); };   // { dg-error "not declared|invalid" }
  [i] { A<decltype(i)>(); };   // { dg-error "invalid" }
}
