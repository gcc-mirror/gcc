// Test P1811 redefinition of class and class template

// { dg-additional-options -fmodules-ts }

module;
#include "gmf-3.h"

// { dg-module-cmi M1 }
export module M1;

// Keep A and B from being discarded per [module.global.frag]
export A f1();
export B<int> f2();
