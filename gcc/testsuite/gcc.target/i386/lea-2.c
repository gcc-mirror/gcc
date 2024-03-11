/* { dg-do compile } */
/* { dg-options "-Oz" } */
int foo(int x) { return x<<2; }
int bar(int x) { return x<<3; }
long long fool(long long x) { return x<<2; }
long long barl(long long x) { return x<<3; }
/* { dg-final { scan-assembler-not "lea\[lq\]" } } */
