/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { *-*-* } { "-march=i[45]86" } { "-O[sz]" } } */
/* { dg-skip-if "" { *-*-* } { "-march=pentium" } { "-O[sz]" } } */
/* { dg-skip-if "" { *-*-* } { "-mtune=i[45]86" } { "-O[sz]" } } */
/* { dg-skip-if "" { *-*-* } { "-mtune=pentium" } { "-O[sz]" } } */

union Data { char a; short b; };

char c;

void val(void) {
  __asm__ __volatile__ ("" : : "r" ((union Data) { c } )); } 

/* { dg-final { scan-assembler "movzbl" } } */
/* { dg-final { scan-assembler-not "xorl" } } */
/* { dg-final { scan-assembler-not "movb" } } */
