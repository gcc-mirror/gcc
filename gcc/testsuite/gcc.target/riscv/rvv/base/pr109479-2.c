/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x_zvl64b -mabi=ilp32d" } */

#include "riscv_vector.h"

void foo0 () {vint64m1_t t;} /* { dg-error {unknown type name 'vint64m1_t'} } */
void foo1 () {vuint64m1_t t;} /* { dg-error {unknown type name 'vuint64m1_t'} } */
void foo2 () {vint64m2_t t;} /* { dg-error {unknown type name 'vint64m2_t'} } */
void foo3 () {vuint64m2_t t;} /* { dg-error {unknown type name 'vuint64m2_t'} } */
void foo4 () {vint64m4_t t;} /* { dg-error {unknown type name 'vint64m4_t'} } */
void foo5 () {vuint64m4_t t;} /* { dg-error {unknown type name 'vuint64m4_t'} } */
void foo6 () {vint64m8_t t;} /* { dg-error {unknown type name 'vint64m8_t'} } */
void foo7 () {vuint64m8_t t;} /* { dg-error {unknown type name 'vuint64m8_t'} } */
