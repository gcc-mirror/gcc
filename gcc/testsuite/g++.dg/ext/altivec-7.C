/* Test for AltiVec type overloading and name mangling.  */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

void foo(vector unsigned char) { }
void foo(vector signed char) { }
void foo(vector bool     char) { }
void foo(vector unsigned short) { }
void foo(vector signed short) { }
void foo(vector bool short) { }
void foo(vector unsigned int) { }
void foo(vector signed int) { }
void foo(vector bool int) { }
void foo(vector float) { }
void foo(vector pixel) { }
void foo(int) { }
void foo(unsigned int) { }
void foo(float) { }

/* { dg-final { scan-assembler "_Z3fooU8__vectorh" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectora" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectorU6__boolc" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectort" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectors" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectorU6__bools" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectorj" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectori" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectorU6__booli" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectorf" } } */
/* { dg-final { scan-assembler "_Z3fooU8__vectoru7__pixel" } } */
/* { dg-final { scan-assembler "_Z3fooi" } } */
/* { dg-final { scan-assembler "_Z3fooj" } } */
/* { dg-final { scan-assembler "_Z3foof" } } */
