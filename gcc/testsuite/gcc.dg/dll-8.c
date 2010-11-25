/* { dg-do compile { target i?86-pc-cygwin } } */
/* { dg-do compile { target i?86-*-mingw* x86_64-*-mingw*} } */
/* { dg-options "-O3 -fwhole-program" } */
/* { dg-final { scan-assembler "foo1" } } */
/* { dg-final { scan-assembler-not "foo2" } } */
/* { dg-final { scan-assembler "doo1" } } */
/* { dg-final { scan-assembler-not "doo2" } } */

__declspec(dllexport) int doo1 = 2;
int doo2 = 3;
__declspec(dllexport) int foo1 (void) { return 0; }
int foo2 (void) { return 1; }
int main() { return 0; }

