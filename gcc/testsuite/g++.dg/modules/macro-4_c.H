// { dg-additional-options "-fmodule-header -Winvalid-imported-macros" }
// { dg-module-cmi {} }

#ifndef MACRO_4_c
#define MACRO_4_c

#undef FIVE // no effect
import "macro-4_a.H";
int a;
#undef THREE
#undef FOUR
#define FOUR 4c

#endif
