/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x -mabi=ilp32d" } */

#include "riscv_vector.h"

void foo0 () {vbool64_t t;} /* { dg-error {unknown type name 'vbool64_t'} } */
void foo1 () {vint8mf8_t t;} /* { dg-error {unknown type name 'vint8mf8_t'} } */
void foo2 () {vuint8mf8_t t;} /* { dg-error {unknown type name 'vuint8mf8_t'} } */
void foo3 () {vint16mf4_t t;} /* { dg-error {unknown type name 'vint16mf4_t'} } */
void foo4 () {vuint16mf4_t t;} /* { dg-error {unknown type name 'vuint16mf4_t'} } */
void foo5 () {vint32mf2_t t;} /* { dg-error {unknown type name 'vint32mf2_t'} } */
void foo6 () {vuint32mf2_t t;} /* { dg-error {unknown type name 'vuint32mf2_t'} } */
void foo7 () {vint64m1_t t;} /* { dg-error {unknown type name 'vint64m1_t'} } */
void foo8 () {vuint64m1_t t;} /* { dg-error {unknown type name 'vuint64m1_t'} } */
void foo9 () {vint64m2_t t;} /* { dg-error {unknown type name 'vint64m2_t'} } */
void foo10 () {vuint64m2_t t;} /* { dg-error {unknown type name 'vuint64m2_t'} } */
void foo11 () {vint64m4_t t;} /* { dg-error {unknown type name 'vint64m4_t'} } */
void foo12 () {vuint64m4_t t;} /* { dg-error {unknown type name 'vuint64m4_t'} } */
void foo13 () {vint64m8_t t;} /* { dg-error {unknown type name 'vint64m8_t'} } */
void foo14 () {vuint64m8_t t;} /* { dg-error {unknown type name 'vuint64m8_t'} } */
