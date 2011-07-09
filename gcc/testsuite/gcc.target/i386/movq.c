/* { dg-do compile }
/* { dg-options "-Os -march=pentium4 -mtune=prescott" } */
/* { dg-require-effective-target ia32 } */

register char foo asm("edi");
char x;
int bar() {
  foo = x;
}
/* { dg-final { scan-assembler "movz" } } */
