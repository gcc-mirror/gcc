/* { dg-do compile }  */
/* { dg-options "-mh -O2 -dp" }  */

long z_si (long a, long b)       { return a != b; }
long sb_si (long a, long b)      { return (long)(a != b) << 15; }
long neg_si (long a, long b)     { return -(a != b); }
long and_si (long a, long b, long c) { return (a != b) & c; }
int and_hi (int a, int b, int c) { return (a != b) & c; }

/* { dg-final { scan-assembler "l=10\\\]\[ \t\]+\\*store_z_si\\M" } }  */
/* { dg-final { scan-assembler "l=10\\\]\[ \t\]+\\*store_z_si_sb\\M" } }  */
/* { dg-final { scan-assembler "l=10\\\]\[ \t\]+\\*store_z_si_neg\\M" } }  */
/* { dg-final { scan-assembler "l=12\\\]\[ \t\]+\\*store_z_si_and\\M" } }  */
/* { dg-final { scan-assembler "l=10\\\]\[ \t\]+\\*store_z_hi_and\\M" } }  */
