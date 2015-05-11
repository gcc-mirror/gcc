/* Octeon targets should use "bbit" instructions for these "if" statements,
   but we test for "bbit" elsewhere.  On other targets, we should implement
   the "if" statements using an "andi" instruction followed by a branch
   on zero.  */
/* { dg-options "forbid_cpu=octeon.*" } */

void bar (int);
NOMIPS16 void f1 (int x) { if (x & 4) bar (1); }
NOMIPS16 void f2 (int x) { if ((x >> 2) & 1) bar (2); }
NOMIPS16 void f3 (unsigned int x) { if (x & 0x10) bar (3); }
NOMIPS16 void f4 (unsigned int x) { if ((x >> 4) & 1) bar (4); }
/* { dg-final { scan-assembler "\tandi\t.*\tandi\t.*\tandi\t.*\tandi\t" } } */
/* { dg-final { scan-assembler-not "\tsrl\t" } } */
/* { dg-final { scan-assembler-not "\tsra\t" } } */
