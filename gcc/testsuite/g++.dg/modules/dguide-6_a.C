// { dg-additional-options "-fmodules" }
// { dg-module-cmi M:a }

module;
#include "dguide-6.h"
export module M:a;
S(int) -> S<int>;
