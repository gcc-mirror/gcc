// Test that P1811 does not allow changing the kind of a template.

// { dg-additional-options "-fmodules" }

module;
#include "p1811-6.h"

// { dg-module-cmi M }
export module M;

export A<int> make_a ();
export B<int> make_b ();
