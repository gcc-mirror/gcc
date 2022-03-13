/* { dg-do compile } */
/* { dg-options "-O2" } */
union Data { char a; short b; };

char c;

void val(void) {
  __asm__ __volatile__ ("" : : "r" ((union Data) { c } )); } 

/* { dg-final { scan-assembler "movzbl" } } */
/* { dg-final { scan-assembler-not "xorl" } } */
/* { dg-final { scan-assembler-not "movb" } } */
