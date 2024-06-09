// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

module;
#include "using-13.h"

export module M;
export using ::A;
export using ::B;
export using ::C;
export using ::D;

#if __cpp_concepts >= 201907L
export using ::E;
#endif
