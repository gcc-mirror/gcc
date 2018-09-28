// { dg-additional-options "-fmodule-legacy=macro-4_c.H -fmodules-atom -fforce-module-macros" }
// { dg-module-bmi {"macro-4_c.H"} }

#undef FIVE // no effect
import "macro-4_a.H";
int a;
#undef THREE
#undef FOUR
#define FOUR 4c
