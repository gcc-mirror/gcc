/* { dg-options "-march=loongson3a" } */

typedef int st;
typedef unsigned int ut;

NOMIPS16 st smul (st x, st y) { return x * y; }
NOMIPS16 st sdiv (st x, st y) { return x / y + x % y; }

NOMIPS16 ut umul (ut x, ut y) { return x * y; }
NOMIPS16 ut udiv (ut x, ut y) { return x / y + x % y; }

/* { dg-final { scan-assembler-times "\tgsmultu\t" 2 } } */
/* { dg-final { scan-assembler-times "\tgsdivu\t" 1 } } */
/* { dg-final { scan-assembler-times "\tgsmodu\t" 1 } } */
/* { dg-final { scan-assembler-times "\tgsdiv\t" 1 } } */
/* { dg-final { scan-assembler-times "\tgsmod\t" 1 } } */
