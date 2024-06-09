/* { dg-do compile } */
/* { dg-options "-O2 -mlarge" } */

const int table[2] = {1, 2};
int foo (char i) { return table[i]; }

/* { dg-final { scan-assembler-not "AND" } } */
/* { dg-final { scan-assembler-not "RRAM" } } */
