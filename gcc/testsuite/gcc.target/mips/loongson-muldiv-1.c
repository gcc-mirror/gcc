/* { dg-options "-O2 isa=loongson" } */

typedef int st;
typedef unsigned int ut;

NOMIPS16 st smul (st x, st y) { return x * y; }
NOMIPS16 st sdiv (st x, st y) { return x / y + x % y; }

NOMIPS16 ut umul (ut x, ut y) { return x * y; }
NOMIPS16 ut udiv (ut x, ut y) { return x / y + x % y; }

/* { dg-final { scan-assembler-times "\tmultu.g\t" 2 } } */
/* { dg-final { scan-assembler-times "\tdivu.g\t" 1 } } */
/* { dg-final { scan-assembler-times "\tmodu.g\t" 1 } } */
/* { dg-final { scan-assembler-times "\tdiv.g\t" 1 } } */
/* { dg-final { scan-assembler-times "\tmod.g\t" 1 } } */
