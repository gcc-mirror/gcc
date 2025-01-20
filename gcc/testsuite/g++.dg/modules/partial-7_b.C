// PR c++/118101
// { dg-additional-options "-fmodules" }
// { dg-module-cmi B }

module;
#include "partial-7.h"
export module B;
import A;
B<int> b;
