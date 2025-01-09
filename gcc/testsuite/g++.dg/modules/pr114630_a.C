// { dg-additional-options "-fmodules" }
// { dg-module-cmi X }

module;
#include "pr114630.h"
export module X;
formatter<int> a;
