/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d -mexplicit-relocs=auto" } */

long a;
int b;
unsigned int c;

long load_a() { return a; }
long load_b() { return b; }
long load_c() { return c; }
void store_a(long x) { a = x; }
void store_b(int x) { b = x; }

/* { dg-final { scan-assembler-not "la.local" } } */
