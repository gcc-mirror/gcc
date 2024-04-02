/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x -mabi=ilp32d" } */

#include "riscv_vector.h"

/* To support target attribute, the vfloat*m*x*_t need to be registered
   in advance.  We add type and args/return value check during the
   set current function but cannot cover below cases.  It is the same
   behavior compared to aarch64 sve.  */
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

void new_foo0 (vbool64_t t) { }    /* { dg-error {argument type 'vbool64_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo1 (vint8mf8_t t) { }   /* { dg-error {argument type 'vint8mf8_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo2 (vuint8mf8_t t) { }  /* { dg-error {argument type 'vuint8mf8_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo3 (vint16mf4_t t) { }  /* { dg-error {argument type 'vint16mf4_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo4 (vuint16mf4_t t) { } /* { dg-error {argument type 'vuint16mf4_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo5 (vint32mf2_t t) { }  /* { dg-error {argument type 'vint32mf2_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo6 (vuint32mf2_t t) { } /* { dg-error {argument type 'vuint32mf2_t' requires the minimal vector length '64' but '32' is given} } */

void new_foo7 (vint64m1_t t) { }   /* { dg-error {argument type 'vint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo8 (vuint64m1_t t) { }  /* { dg-error {argument type 'vuint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo9 (vint64m2_t t) { }   /* { dg-error {argument type 'vint64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo10 (vuint64m2_t t) { } /* { dg-error {argument type 'vuint64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo11 (vint64m4_t t) { }  /* { dg-error {argument type 'vint64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo12 (vuint64m4_t t) { } /* { dg-error {argument type 'vuint64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo13 (vint64m8_t t) { }  /* { dg-error {argument type 'vint64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo14 (vuint64m8_t t) { } /* { dg-error {argument type 'vuint64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
