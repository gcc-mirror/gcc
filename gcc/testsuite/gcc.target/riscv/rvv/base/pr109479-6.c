/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x_zvl64b -mabi=ilp32d" } */

#include "riscv_vector.h"

void foo0 () {vbool64_t t;}
void foo1 () {vint8mf8_t t;}
void foo2 () {vuint8mf8_t t;}
void foo3 () {vint16mf4_t t;}
void foo4 () {vuint16mf4_t t;}
void foo5 () {vint32mf2_t t;}
void foo6 () {vuint32mf2_t t;}
void foo7 () {vint64m1_t t;}
void foo8 () {vuint64m1_t t;}
void foo9 () {vint64m2_t t;}
void foo10 () {vuint64m2_t t;}
void foo11 () {vint64m4_t t;}
void foo12 () {vuint64m4_t t;}
void foo13 () {vint64m8_t t;}
void foo14 () {vuint64m8_t t;}

void new_foo0 (vbool64_t t) { }
void new_foo1 (vint8mf8_t t) { }
void new_foo2 (vuint8mf8_t t) { }
void new_foo3 (vint16mf4_t t) { }
void new_foo4 (vuint16mf4_t t) { }
void new_foo5 (vint32mf2_t t) { }
void new_foo6 (vuint32mf2_t t) { }

void new_foo7 (vint64m1_t t) { }   /* { dg-error {argument type 'vint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo8 (vuint64m1_t t) { }  /* { dg-error {argument type 'vuint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo9 (vint64m2_t t) { }   /* { dg-error {argument type 'vint64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo10 (vuint64m2_t t) { } /* { dg-error {argument type 'vuint64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo11 (vint64m4_t t) { }  /* { dg-error {argument type 'vint64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo12 (vuint64m4_t t) { } /* { dg-error {argument type 'vuint64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo13 (vint64m8_t t) { }  /* { dg-error {argument type 'vint64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo14 (vuint64m8_t t) { } /* { dg-error {argument type 'vuint64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
