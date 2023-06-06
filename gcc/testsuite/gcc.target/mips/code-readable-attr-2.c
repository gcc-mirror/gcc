/* { dg-options "(-mips16) -mgp32 addressing=absolute" } */

volatile int x1;
volatile int x2;
volatile int x3;
volatile int x4;
volatile int x5;
volatile int x6;
volatile int x7;
volatile int x8;
volatile int x9;
volatile int x10;
volatile int x11;

MIPS16 __attribute__((code_readable ("pcrel"))) int
foo (int i, volatile int *x)
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
    case 8: return x8 + x[7];
    case 9: return x9 + x[8];
    case 10: return x10 + x[9];
    case 11: return x11 + x[10];
    default: return 0;
    }
}

extern int k[];

MIPS16 __attribute__((code_readable ("pcrel"))) int *
bar (void)
{
  return k;
}

/* { dg-final { scan-assembler-not "\tla\t" } } */
/* { dg-final { scan-assembler-not "\t\\.half\t" } } */
/* { dg-final { scan-assembler "\t\\.word\t\[^\n\]*L" } } */

/* { dg-final { scan-assembler "\t\\.word\tk\n" } } */
/* { dg-final { scan-assembler-not "%hi\\(k\\)" } } */
/* { dg-final { scan-assembler-not "%lo\\(k\\)" } } */
