template <class T> struct A { };

template
__attribute__ ((packed))
struct A<int>;			// { dg-warning "attribute" }
