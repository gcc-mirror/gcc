/* Test for AltiVec type overloading and name mangling.  */
/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

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

/* { dg-final { scan-assembler "_Z3fooDv16_h" } } */
/* { dg-final { scan-assembler "_Z3fooDv16_a" } } */
/* { dg-final { scan-assembler "_Z3fooDv16_U6__boolc" } } */
/* { dg-final { scan-assembler "_Z3fooDv8_t" } } */
/* { dg-final { scan-assembler "_Z3fooDv8_s" } } */
/* { dg-final { scan-assembler "_Z3fooDv8_U6__bools" } } */
/* { dg-final { scan-assembler "_Z3fooDv4_j" } } */
/* { dg-final { scan-assembler "_Z3fooDv4_i" } } */
/* { dg-final { scan-assembler "_Z3fooDv4_U6__booli" } } */
/* { dg-final { scan-assembler "_Z3fooDv4_f" } } */
/* { dg-final { scan-assembler "_Z3fooDv8_u7__pixel" } } */
/* { dg-final { scan-assembler "_Z3fooi" } } */
/* { dg-final { scan-assembler "_Z3fooj" } } */
/* { dg-final { scan-assembler "_Z3foof" } } */
