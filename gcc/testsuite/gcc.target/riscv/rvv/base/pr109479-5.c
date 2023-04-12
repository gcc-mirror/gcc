/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x_zvl64b -mabi=ilp32d" } */

#include "riscv_vector.h"

void foo0 () {__rvv_bool64_t t;}
void foo1 () {__rvv_int8mf8_t t;}
void foo2 () {__rvv_uint8mf8_t t;}
void foo3 () {__rvv_int16mf4_t t;}
void foo4 () {__rvv_uint16mf4_t t;}
void foo5 () {__rvv_int32mf2_t t;}
void foo6 () {__rvv_uint32mf2_t t;}
void foo7 () {__rvv_int64m1_t t;} /* { dg-error {unknown type name '__rvv_int64m1_t'} } */
void foo8 () {__rvv_uint64m1_t t;} /* { dg-error {unknown type name '__rvv_uint64m1_t'} } */
void foo9 () {__rvv_int64m2_t t;} /* { dg-error {unknown type name '__rvv_int64m2_t'} } */
void foo10 () {__rvv_uint64m2_t t;} /* { dg-error {unknown type name '__rvv_uint64m2_t'} } */
void foo11 () {__rvv_int64m4_t t;} /* { dg-error {unknown type name '__rvv_int64m4_t'} } */
void foo12 () {__rvv_uint64m4_t t;} /* { dg-error {unknown type name '__rvv_uint64m4_t'} } */
void foo13 () {__rvv_int64m8_t t;} /* { dg-error {unknown type name '__rvv_int64m8_t'} } */
void foo14 () {__rvv_uint64m8_t t;} /* { dg-error {unknown type name '__rvv_uint64m8_t'} } */
