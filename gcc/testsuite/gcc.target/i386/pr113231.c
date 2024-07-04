/* { dg-do compile } */
/* { dg-options "-Os" } */

void foo(int *i) { *i *= 2; }
void bar(int *i) { *i <<= 2; }
void baz(int *i) { *i >>= 2; }

/* { dg-final { scan-assembler-not "movd" } } */
