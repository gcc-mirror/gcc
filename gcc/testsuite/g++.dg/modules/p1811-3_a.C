// Test recovery from an incomplete redundant class definition.

// { dg-additional-options "-fmodules" }

module;
#include "p1811-3.h"

// { dg-module-cmi M }
export module M;

export A make_a ();
