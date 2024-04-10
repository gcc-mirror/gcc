// PR c++/113405
// { dg-additional-options "-fmodules-ts" }
// { dg-require-effective-target c++20 }
// { dg-module-cmi M }

module;

#include "concept-9.h"

export module M;

export template<class T>
using corge_alias = corge<T>::alias;
