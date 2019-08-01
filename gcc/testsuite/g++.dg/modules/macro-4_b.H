// { dg-additional-options "-fmodule-header -Winvalid-imported-macros" }
// { dg-module-cmi {} }

#ifndef MACRO_4_b
#define MACRO_4_b

#define ONE 1
#define TWO 2a
#undef THREE // no effect
#define THREE 3b

#endif
