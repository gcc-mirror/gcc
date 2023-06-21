/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x_zvl64b -mabi=ilp32d" } */

#include "riscv_vector.h"

void foo0 () {__rvv_int64m1_t t;} /* { dg-error {unknown type name '__rvv_int64m1_t'} } */
void foo1 () {__rvv_uint64m1_t t;} /* { dg-error {unknown type name '__rvv_uint64m1_t'} } */
void foo2 () {__rvv_int64m2_t t;} /* { dg-error {unknown type name '__rvv_int64m2_t'} } */
void foo3 () {__rvv_uint64m2_t t;} /* { dg-error {unknown type name '__rvv_uint64m2_t'} } */
void foo4 () {__rvv_int64m4_t t;} /* { dg-error {unknown type name '__rvv_int64m4_t'} } */
void foo5 () {__rvv_uint64m4_t t;} /* { dg-error {unknown type name '__rvv_uint64m4_t'} } */
void foo6 () {__rvv_int64m8_t t;} /* { dg-error {unknown type name '__rvv_int64m8_t'} } */
void foo7 () {__rvv_uint64m8_t t;} /* { dg-error {unknown type name '__rvv_uint64m8_t'} } */
