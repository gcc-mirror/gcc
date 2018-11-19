// { dg-additional-options "-fmodule-legacy -fforce-module-macros" }
// { dg-module-bmi {"macro-4_c.H"} }

#ifndef MACRO_4_c
#define MACRO_4_c

#undef FIVE // no effect
import "macro-4_a.H";
int a;
#undef THREE
#undef FOUR
#define FOUR 4c

#endif
