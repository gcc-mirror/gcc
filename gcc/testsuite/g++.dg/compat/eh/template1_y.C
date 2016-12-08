#include "template1.h"

template<class T> void C<T>::f (void)
#if __cplusplus < 201103L
throw (E)
#endif
{
  throw E();
}

template class C<int>;
