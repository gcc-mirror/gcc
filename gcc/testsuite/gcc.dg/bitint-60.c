/* PR tree-optimization/112941 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O2 -std=c23" } */

unsigned _BitInt(495) f1 (signed _BitInt(381) x) { unsigned _BitInt(539) y = x; return y; }
unsigned _BitInt(495) f2 (unsigned _BitInt(381) x) { unsigned _BitInt(539) y = x; return y; }
unsigned _BitInt(495) f3 (signed _BitInt(381) x) { _BitInt(539) y = x; return y; }
unsigned _BitInt(495) f4 (unsigned _BitInt(381) x) { _BitInt(539) y = x; return y; }
_BitInt(495) f5 (signed _BitInt(381) x) { unsigned _BitInt(539) y = x; return y; }
_BitInt(495) f6 (unsigned _BitInt(381) x) { unsigned _BitInt(539) y = x; return y; }
_BitInt(495) f7 (signed _BitInt(381) x) { _BitInt(539) y = x; return y; }
_BitInt(495) f8 (unsigned _BitInt(381) x) { _BitInt(539) y = x; return y; }
unsigned _BitInt(495) f9 (signed _BitInt(381) x) { return (unsigned _BitInt(539)) x; }
unsigned _BitInt(495) f10 (unsigned _BitInt(381) x) { return (unsigned _BitInt(539)) x; }
unsigned _BitInt(495) f11 (signed _BitInt(381) x) { return (_BitInt(539)) x; }
unsigned _BitInt(495) f12 (unsigned _BitInt(381) x) { return (_BitInt(539)) x; }
_BitInt(495) f13 (signed _BitInt(381) x) { return (unsigned _BitInt(539)) x; }
_BitInt(495) f14 (unsigned _BitInt(381) x) { return (unsigned _BitInt(539)) x; }
_BitInt(495) f15 (signed _BitInt(381) x) { return (_BitInt(539)) x; }
_BitInt(495) f16 (unsigned _BitInt(381) x) { return (_BitInt(539)) x; }
