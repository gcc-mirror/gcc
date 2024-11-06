// PR c++/113814
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

#include "partial-6.h"

template <typename T>
struct A<T*> { int a; };

template <template <typename> typename TT>
struct B<TT, int> { int b; };
