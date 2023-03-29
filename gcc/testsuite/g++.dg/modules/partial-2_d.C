// PR c++/107033
// { dg-additional-options -fmodules-ts }
// { dg-module-cmi pr107033 }
export module pr107033;

import "partial-2_c.H";

#include "partial-2.cc"
