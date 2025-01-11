// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

module;
#include "dguide-6.h"
export module M;
export import :a;
S(int, int) -> S<double>;
