#include <iostream.h>
#include <typeinfo>

template <typename T>
class A {
public:
  void test ();
};

template <typename T>
void
A<T>::test(){
  cerr << "test for " << typeid(*this).name() << endl;
}
// Specialization declaration
void                           
A<double>::test();		// ERROR - not a specialization

// Specialization definition
void
A<double>::test(){		// ERROR - not a specialization
  cerr << "specialization for " << typeid(*this).name() << endl;
}


int
main(){
  A<int> ai;
  A<double> ad;
  ai.test();
  ad.test();
  return 0;
}

