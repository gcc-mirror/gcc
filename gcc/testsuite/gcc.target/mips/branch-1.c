/* Octeon targets should use "bbit" instructions for these "if" statements,
   but we test for "bbit" elsewhere.  On other targets, we should implement
   the "if" statements using an "andi" instruction followed by a branch
   on zero.  */
/* { dg-options "-O2 forbid_cpu=octeon.*" } */

void bar (void);
NOMIPS16 void f1 (int x) { if (x & 4) bar (); }
NOMIPS16 void f2 (int x) { if ((x >> 2) & 1) bar (); }
NOMIPS16 void f3 (unsigned int x) { if (x & 0x10) bar (); }
NOMIPS16 void f4 (unsigned int x) { if ((x >> 4) & 1) bar (); }
/* { dg-final { scan-assembler "\tandi\t.*\tandi\t.*\tandi\t.*\tandi\t" } } */
/* { dg-final { scan-assembler-not "\tsrl\t" } } */
/* { dg-final { scan-assembler-not "\tsra\t" } } */
