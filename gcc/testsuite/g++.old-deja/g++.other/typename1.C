// { dg-do assemble  }
// { dg-options "-pedantic -Wno-deprecated" }
// This code snippet should be rejected with -pedantic
// Based on a test case by Louidor Erez <s3824888@techst02.technion.ac.il>


template<class T>
class Vector {
public:
  typedef T* iterator;
};

template<class T>
void f()
{
  Vector<T>::iterator i = 0; // { dg-error "typename" "typename" } missing typename
} // { dg-error "expected" "expected" { target *-*-* } .-1 }
