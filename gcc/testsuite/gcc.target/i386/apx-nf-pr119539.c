/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapx-features=nf -march=x86-64 -O2" } */
/* { dg-final { scan-assembler-times "\{nf\} rol" 2 } } */

long int f1 (int x) { return ~(1ULL << (x & 0x3f)); }
long int f2 (char x) { return ~(1ULL << (x & 0x3f)); }
