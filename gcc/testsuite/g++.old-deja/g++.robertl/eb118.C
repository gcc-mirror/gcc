// Test for obsolete specialization syntax.  Turn off -pedantic.
// Special g++ Options:

#include <iostream>
#include <typeinfo>

template <typename T>
class A {
public:
  void test ();
};

template <typename T>
void
A<T>::test(){
  std::cerr << "test for " << typeid(*this).name() << std::endl;
}
// Specialization declaration
void                           
A<double>::test();

// Specialization definition
void
A<double>::test(){
  std::cerr << "specialization for " << typeid(*this).name() << std::endl;
}


int
main(){
  A<int> ai;
  A<double> ad;
  ai.test();
  ad.test();
  return 0;
}


