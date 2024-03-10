/* { dg-do compile } */
/* { dg-options "-O2 -march=la464 -mabi=lp64d -mstrict-align" } */
/* { dg-final { scan-assembler-not "vst" } } */

extern char a[], b[];
void test() { __builtin_memcpy(a, b, 32); }
