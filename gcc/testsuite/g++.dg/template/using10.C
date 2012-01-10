// PR c++/22136

struct B { 
  void foo(); 
}; 
 
template <typename T> class I : public B {}; 
 
template <typename T> class D : private I<T> { 
  I<T>::B::foo; // { dg-warning "deprecated" } 
}; 
