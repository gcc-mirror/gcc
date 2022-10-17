/* { dg-do compile } */
/* { dg-options "-Oz" } */
short s;
int i;
long long l;

void s0() { s = 0; }
void sm1() { s = -1; }
void i0() { i = 0; }
void im1() { i = -1; }
void l0() { l = 0; }
void lm1() { l = -1; }

/* { dg-final { scan-assembler-not "\tmov\[wlq\]\t\\\$0," } } */
/* { dg-final { scan-assembler-not "\tmov\[wlq\]\t\\\$-1," } } */
/* { dg-final { scan-assembler "\tandw\t\\\$0," } } */
/* { dg-final { scan-assembler "\torw\t\\\$-1," } } */
/* { dg-final { scan-assembler "\torl\t\\\$-1," } } */

