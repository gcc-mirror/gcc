// PR c++/124888
// { dg-additional-options -fmodules }

module;
#include <typeinfo>
export module foo;

export template <class T>
const std::type_info &f(T *p)
{
  return typeid (*p);
}
