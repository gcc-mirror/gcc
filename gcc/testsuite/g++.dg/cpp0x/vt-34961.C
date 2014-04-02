// { dg-do compile { target c++11 } }
template<typename... T> struct A
{
  static const int i __attribute__((aligned(__alignof(T)))) = 0; // { dg-error "not expanded|T" }
}; 

A<int> a; 
