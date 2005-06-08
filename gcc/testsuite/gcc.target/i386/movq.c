/* { dg-do compile { target "i?86*-*-*" } }
/* { dg-options "-Os -march=pentium4 -mtune=prescott" } */
register char foo asm("edi");
char x;
int bar() {
  foo = x;
}
/* { dg-final { scan-assembler "movz" } } */
