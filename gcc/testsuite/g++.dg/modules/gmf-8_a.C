// PR c++/127787
// { dg-additional-options -fmodules }

module;
#include "gmf-8.h"
export module M;
export using N::A;
