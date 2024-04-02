/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x_zvl64b -mabi=ilp32d" } */

#include "riscv_vector.h"

/* To support target attribute, the vfloat*m*x*_t need to be registered
   in advance.  We add type and args/return value check during the
   set current function but cannot cover below cases.  It is the same
   behavior compared to aarch64 sve.  */
void foo0 () {__rvv_int64m1_t t;}
void foo1 () {__rvv_uint64m1_t t;}
void foo2 () {__rvv_int64m2_t t;}
void foo3 () {__rvv_uint64m2_t t;}
void foo4 () {__rvv_int64m4_t t;}
void foo5 () {__rvv_uint64m4_t t;}
void foo6 () {__rvv_int64m8_t t;}
void foo7 () {__rvv_uint64m8_t t;}

void new_foo0 (__rvv_int64m1_t t) { }  /* { dg-error {argument type '__rvv_int64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo1 (__rvv_uint64m1_t t) { } /* { dg-error {argument type '__rvv_uint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo2 (__rvv_int64m2_t t) { }  /* { dg-error {argument type '__rvv_int64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo3 (__rvv_uint64m2_t t) { } /* { dg-error {argument type '__rvv_uint64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo4 (__rvv_int64m4_t t) { }  /* { dg-error {argument type '__rvv_int64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo5 (__rvv_uint64m4_t t) { } /* { dg-error {argument type '__rvv_uint64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo6 (__rvv_int64m8_t t) { }  /* { dg-error {argument type '__rvv_int64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo7 (__rvv_uint64m8_t t) { } /* { dg-error {argument type '__rvv_uint64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
