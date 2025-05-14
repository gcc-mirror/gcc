/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -march=z17" } */
/* { dg-final { scan-assembler-times {vecq\t} 8 } } */
/* { dg-final { scan-assembler-times {veclq\t} 4 } } */
/* { dg-final { scan-assembler-times {locghile\t} 1 } } LE */
/* { dg-final { scan-assembler-times {slbgr\t} 1 } } LEU */
/* { dg-final { scan-assembler-times {locghil\t} 2 } } LT LTU */
/* { dg-final { scan-assembler-times {locghihe\t} 2 } } GE GEU */
/* { dg-final { scan-assembler-times {locghih\t} 1 } } GT */
/* { dg-final { scan-assembler-times {alcgr\t} 1 } } GTU */
/* { dg-final { scan-assembler-times {locghie\t} 2 } } EQ EQU */
/* { dg-final { scan-assembler-times {locghine\t} 2 } } NE NEU */

int test_le (__int128 x, __int128 y) { return x <= y; }
int test_leu (unsigned __int128 x, unsigned __int128 y) { return x <= y; }
int test_lt (__int128 x, __int128 y) { return x < y; }
int test_ltu (unsigned __int128 x, unsigned __int128 y) { return x < y; }
int test_ge (__int128 x, __int128 y) { return x >= y; }
int test_geu (unsigned __int128 x, unsigned __int128 y) { return x >= y; }
int test_gt (__int128 x, __int128 y) { return x > y; }
int test_gtu (unsigned __int128 x, unsigned __int128 y) { return x > y; }
int test_eq (__int128 x, __int128 y) { return x == y; }
int test_equ (unsigned __int128 x, unsigned __int128 y) { return x == y; }
int test_ne (__int128 x, __int128 y) { return x != y; }
int test_neu (unsigned __int128 x, unsigned __int128 y) { return x != y; }
