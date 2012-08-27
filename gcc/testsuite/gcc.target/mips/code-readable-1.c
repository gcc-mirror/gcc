/* { dg-options "(-mips16) -mcode-readable=yes -mgp32 addressing=absolute" } */
/* { dg-skip-if ".half requires -O" { *-*-* } { "-O0" } { "" } } */

volatile int x1;
volatile int x2;
volatile int x3;
volatile int x4;
volatile int x5;
volatile int x6;
volatile int x7;

MIPS16 int
foo (int i, volatile *x)
{
  switch (i)
    {
    case 1: return x1 + x[0];
    case 2: return x2 + x[1];
    case 3: return x3 + x[2];
    case 4: return x4 + x[3];
    case 5: return x5 + x[4];
    case 6: return x6 + x[5];
    case 7: return x7 + x[6];
    default: return 0;
    }
}

extern int k[];

MIPS16 int *
bar (void)
{
  return k;
}

/* { dg-final { scan-assembler "\tla\t" } } */
/* { dg-final { scan-assembler "\t\\.half\t" } } */
/* { dg-final { scan-assembler-not "%hi\\(\[^)\]*L" } } */
/* { dg-final { scan-assembler-not "%lo\\(\[^)\]*L" } } */

/* { dg-final { scan-assembler "\t\\.word\tk\n" } } */
/* { dg-final { scan-assembler-not "%hi\\(k\\)" } } */
/* { dg-final { scan-assembler-not "%lo\\(k\\)" } } */
