// { dg-additional-options "-fmodule-legacy=macro-4_b.H -fmodules-atom -fforce-module-macros" }
// { dg-module-bmi {"macro-4_b.H"} }

#ifndef MACRO_4_b
#define MACRO_4_b

#define ONE 1
#define TWO 2a
#undef THREE // no effect
#define THREE 3b

#endif
