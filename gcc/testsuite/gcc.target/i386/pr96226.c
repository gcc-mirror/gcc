/* PR target/96226 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "\troll\t" 4 } } */
/* { dg-final { scan-assembler-times "\trolq\t" 4 { target { ! ia32 } } } } */

int f1 (int x) { return ~(1U << (x & 0x1f)); }
int f2 (int x) { return ~(1U << x); }
int f3 (unsigned char *x) { return ~(1U << (x[0] & 0x1f)); }
int f4 (unsigned char *x) { return ~(1U << x[0]); }
#ifdef __x86_64__
long int f5 (int x) { return ~(1ULL << (x & 0x3f)); }
long int f6 (int x) { return ~(1ULL << x); }
long int f7 (unsigned char *x) { return ~(1ULL << (x[0] & 0x3f)); }
long int f8 (unsigned char *x) { return ~(1ULL << x[0]); }
#endif
