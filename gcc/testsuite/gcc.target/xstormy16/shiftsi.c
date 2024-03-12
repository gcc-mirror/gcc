/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned long ashift_1(unsigned long x) { return x << 1; }
unsigned long ashift_2(unsigned long x) { return x << 2; }
unsigned long lshiftrt_1(unsigned long x) { return x >> 1; }
unsigned long lshiftrt_2(unsigned long x) { return x >> 2; }
long ashiftrt_1(long x) { return x >> 1; }
long ashiftrt_2(long x) { return x >> 2; }

/* { dg-final { scan-assembler-not "mov " } } */
/* { dg-final { scan-assembler-not "or " } } */
