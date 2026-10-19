// Test that P1811 does not allow multiple definitions in one TU.

// { dg-additional-options "-fmodules" }

module;
#include "p1811-5.h"

// { dg-module-cmi M }
export module M;

export A make_a ();
