/* PR tree-optimization/112566 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-mbmi2 -mlzcnt -mpopcnt" { target i?86-*-* x86_64-*-* } } */
/* { dg-final { scan-tree-dump-not "ll \\\(" "optimized" { target ia32 } } } */
/* { dg-final { scan-tree-dump-not "\\\(long long (unsigned )?int\\\)" "optimized" { target ia32 } } } */

int foo (unsigned int x) { return __builtin_ctzll (x); }
int bar (unsigned int x) { return __builtin_popcountll (x); }
int baz (unsigned int x) { return __builtin_parityll (x); }
int qux (int x) { return __builtin_ffsll (x); }
int corge (int x) { return __builtin_ctzll (x); }
int garply (int x) { return __builtin_parityll (x); }
int fred (unsigned int x) { return __builtin_ffsll (x); }
