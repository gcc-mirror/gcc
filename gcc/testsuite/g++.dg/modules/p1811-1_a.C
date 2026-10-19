// Test P1811 redefinition of a function-try-block.

// { dg-additional-options "-fmodules" }

module;
#include "p1811-1.h"

// { dg-module-cmi M }
export module M;

export inline auto pf = &f;
