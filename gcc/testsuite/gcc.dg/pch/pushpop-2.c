/* { dg-options -std=c11 } */
#include "pushpop-2.hs"

#if π != 4
#error π != 4
#endif
#pragma pop_macro("\u03C0")
#if π != 3
#error π != 3
#endif

#if \u03B1 != 6
#error α != 6
#endif
_Pragma("pop_macro(\"\\u03B1\")")
#if α != 5
#error α != 5
#endif
