// PR c++/63693
// { dg-do compile { target c++11 } }

template<typename T>
class C{
 T t;
  decltype(t)::a:: // { dg-error "expected" }
// { dg-error "-:expected" "" { target *-*-* } .+1 }
