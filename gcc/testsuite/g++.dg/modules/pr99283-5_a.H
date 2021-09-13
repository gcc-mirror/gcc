// PR 99283 part 5
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

#include "pr99283-5.h"

template<typename _Value>
const int __traits<_Value>::__digits;

template<typename _Tp>
void Foo ()
{
  __traits<unsigned>::__digits;
}
