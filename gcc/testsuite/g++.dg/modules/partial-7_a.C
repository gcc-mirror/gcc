// PR c++/118101
// { dg-additional-options "-fmodules" }
// { dg-module-cmi A }

module;
#include "partial-7.h"
export module A;
B<int> a;
