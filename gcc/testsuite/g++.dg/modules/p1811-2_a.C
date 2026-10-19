// Test P1811 redefinition of a qualified enum.

// { dg-additional-options "-fmodules" }

module;
#include "p1811-2.h"

// { dg-module-cmi M }
export module M;

export C make_c ();
