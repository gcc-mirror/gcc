/* { dg-options "-march=loongson3a -mgp64" } */

typedef long long st;
typedef unsigned long long ut;

NOMIPS16 st smul (st x, st y) { return x * y; }
NOMIPS16 st sdiv (st x, st y) { return x / y + x % y; }

NOMIPS16 ut umul (ut x, ut y) { return x * y; }
NOMIPS16 ut udiv (ut x, ut y) { return x / y + x % y; }

/* { dg-final { scan-assembler-times "\tgsdmultu\t" 2 } } */
/* { dg-final { scan-assembler-times "\tgsddivu\t" 1 } } */
/* { dg-final { scan-assembler-times "\tgsdmodu\t" 1 } } */
/* { dg-final { scan-assembler-times "\tgsddiv\t" 1 } } */
/* { dg-final { scan-assembler-times "\tgsdmod\t" 1 } } */
