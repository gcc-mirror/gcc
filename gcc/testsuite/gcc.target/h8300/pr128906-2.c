/* The inverted variants of *store_z_<mode>_and used to emit "X0" instead of
   "%X0" as the bist operand.  */
/* { dg-do compile }  */
/* { dg-options "-mh -O2" }  */

int  and_hi_ne (int a, int b, int c)    { return (a != b) & c; }
long and_si_ne (long a, long b, long c) { return (a != b) & c; }

/* { dg-final { scan-assembler-times "bist\t#0,r0l" 2 } }  */
/* { dg-final { scan-assembler-not "X0" } }  */
