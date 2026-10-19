// Test that a forward declaration of a class (template) in the global module
// combines with a reachable imported definition.

// { dg-additional-options -fmodules-ts }

module;
#include "gmf-3.h"

// { dg-module-cmi M1 }
export module M1;

// Keep A and B from being discarded per [module.global.frag]
export A f1();
export B<int> f2();
