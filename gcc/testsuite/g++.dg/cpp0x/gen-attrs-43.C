// { dg-do compile { target c++11 } }
template <class T> struct A { };

template [[gnu::packed]] struct A<int>;  // { dg-warning "ignored in explicit instantiation" }
