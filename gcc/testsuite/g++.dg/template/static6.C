// PR c++/13969

struct B { 
  static const int N=10; 
}; 
 
template <int> struct X {}; 
 
template <typename> struct S { 
  static const int N = B::N; 
  X<N> x; 
}; 
 
template class S<float>; 
