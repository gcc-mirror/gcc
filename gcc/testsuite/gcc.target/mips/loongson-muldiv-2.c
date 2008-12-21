/* { dg-options "-O2 isa=loongson -mgp64" } */

typedef long long st;
typedef unsigned long long ut;

NOMIPS16 st smul (st x, st y) { return x * y; }
NOMIPS16 st sdiv (st x, st y) { return x / y + x % y; }

NOMIPS16 ut umul (ut x, ut y) { return x * y; }
NOMIPS16 ut udiv (ut x, ut y) { return x / y + x % y; }

/* { dg-final { scan-assembler-times "\tdmultu.g\t" 2 } } */
/* { dg-final { scan-assembler-times "\tddivu.g\t" 1 } } */
/* { dg-final { scan-assembler-times "\tdmodu.g\t" 1 } } */
/* { dg-final { scan-assembler-times "\tddiv.g\t" 1 } } */
/* { dg-final { scan-assembler-times "\tdmod.g\t" 1 } } */
