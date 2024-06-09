/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapx-features=egpr,push2pop2,ndd,ppx,nf -march=x86-64 -O2" } */
/* { dg-final { scan-assembler-times "\{nf\} and" 1 } } */
/* { dg-final { scan-assembler-times "\{nf\} or" 1 } } */
/* { dg-final { scan-assembler-times "\{nf\} rol" 4 } } */

struct B { unsigned bit0 : 1; unsigned bit1 : 1; };

void
foo (struct B *b)
{
    b->bit0 = b->bit0 | b->bit1;
}
long int f1 (int x) { return ~(1ULL << (x & 0x3f)); }
long int f2 (int x) { return ~(1ULL << x); }
long int f3 (unsigned char *x) { return ~(1ULL << (x[0] & 0x3f)); }
long int f4 (unsigned char *x) { return ~(1ULL << x[0]); }
