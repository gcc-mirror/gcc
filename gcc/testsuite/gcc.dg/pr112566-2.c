/* PR tree-optimization/112566 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O2 -fdump-tree-ccp2" } */
/* { dg-final { scan-tree-dump-not "\\\((unsigned )?_BitInt\\\(512\\\)\\\)" "ccp2" } } */

int foo (unsigned _BitInt(256) x) { return __builtin_ctzg ((unsigned _BitInt(512)) x); }
int bar (unsigned _BitInt(256) x) { return __builtin_popcountg ((unsigned _BitInt(512)) x); }
int baz (unsigned _BitInt(256) x) { return __builtin_parityg ((unsigned _BitInt(512)) x); }
int qux (_BitInt(256) x) { return __builtin_ffsg ((_BitInt(512)) x); }
int corge (_BitInt(256) x) { return __builtin_ctzg ((unsigned _BitInt(512)) x); }
int garply (_BitInt(256) x) { return __builtin_parityg ((unsigned _BitInt(512)) x); }
int fred (unsigned _BitInt(256) x) { return __builtin_ffsg ((_BitInt(512)) x); }
