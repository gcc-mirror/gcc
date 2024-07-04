/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x_zvl64b -mabi=ilp32d" } */

#include "riscv_vector.h"

/* To support target attribute, the vfloat*m*x*_t need to be registered
   in advance.  We add type and args/return value check during the
   set current function but cannot cover below cases.  It is the same
   behavior compared to aarch64 sve.  */
void foo0 () {vint64m1_t t;}
void foo1 () {vuint64m1_t t;}
void foo2 () {vint64m2_t t;}
void foo3 () {vuint64m2_t t;}
void foo4 () {vint64m4_t t;}
void foo5 () {vuint64m4_t t;}
void foo6 () {vint64m8_t t;}
void foo7 () {vuint64m8_t t;}

void new_foo0 (vint64m1_t t) { }  /* { dg-error {argument type 'vint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo1 (vuint64m1_t t) { } /* { dg-error {argument type 'vuint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo2 (vint64m2_t t) { }  /* { dg-error {argument type 'vint64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo3 (vuint64m2_t t) { } /* { dg-error {argument type 'vuint64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo4 (vint64m4_t t) { }  /* { dg-error {argument type 'vint64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo5 (vuint64m4_t t) { } /* { dg-error {argument type 'vuint64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo6 (vint64m8_t t) { }  /* { dg-error {argument type 'vint64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo7 (vuint64m8_t t) { } /* { dg-error {argument type 'vuint64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
