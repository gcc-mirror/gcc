// PR c++/109679
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

module;
#include "using-11.h"

export module M;
export using ::foo;
