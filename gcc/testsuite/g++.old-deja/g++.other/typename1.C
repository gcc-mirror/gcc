// This code snippet should be rejected with -pedantic
// Based on a test case by Louidor Erez <s3824888@techst02.technion.ac.il>

// Build don't link:
// Special g++ Options: -pedantic -Wno-deprecated

template<class T>
class Vector {
public:
  typedef T* iterator;
};

template<class T>
void f()
{
  Vector<T>::iterator i = 0; // WARNING - missing typename
}
