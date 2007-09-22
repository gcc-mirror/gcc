/* We should implement these "if" statements using an "andi" instruction
   followed by a branch on zero.  */
/* { dg-mips-options "-O2" } */

void bar (void);
NOMIPS16 void f1 (int x) { if (x & 4) bar (); }
NOMIPS16 void f2 (int x) { if ((x >> 2) & 1) bar (); }
NOMIPS16 void f3 (unsigned int x) { if (x & 0x10) bar (); }
NOMIPS16 void f4 (unsigned int x) { if ((x >> 4) & 1) bar (); }
/* { dg-final { scan-assembler "\tandi\t.*\tandi\t.*\tandi\t.*\tandi\t" } } */
/* { dg-final { scan-assembler-not "\tsrl\t" } } */
/* { dg-final { scan-assembler-not "\tsra\t" } } */
