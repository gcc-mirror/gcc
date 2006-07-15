//PR c++/28269

template<int> struct A;

struct __attribute__((unused)) A<0<; // { dg-error "template argument|unqualified-id" }
