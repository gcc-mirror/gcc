/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */
int eq(__int128 x, __int128 y) { return x == y; }
int ne(__int128 x, __int128 y) { return x != y; }
/* { dg-final { scan-assembler-times "xorq" 4 } } */
/* { dg-final { scan-assembler-times "setne" 1 } } */
/* { dg-final { scan-assembler-times "sete" 1 } } */

