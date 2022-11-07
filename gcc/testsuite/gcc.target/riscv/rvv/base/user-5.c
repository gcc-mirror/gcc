/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x -mabi=ilp32d" } */

#include "riscv_vector.h"

void foo0 () {vbool64_t t;} /* { dg-error {unknown type name 'vbool64_t'} } */
void foo1 () {vbool32_t t;}
void foo2 () {vbool16_t t;}
void foo3 () {vbool8_t t;}
void foo4 () {vbool4_t t;}
void foo5 () {vbool2_t t;}
void foo6 () {vbool1_t t;}
void foo7 () {vint8mf8_t t;} /* { dg-error {unknown type name 'vint8mf8_t'} } */
void foo8 () {vuint8mf8_t t;} /* { dg-error {unknown type name 'vuint8mf8_t'} } */
void foo9 () {vint8mf4_t t;}
void foo10 () {vuint8mf4_t t;}
void foo11 () {vint8mf2_t t;}
void foo12 () {vuint8mf2_t t;}
void foo13 () {vint8m1_t t;}
void foo14 () {vuint8m1_t t;}
void foo15 () {vint8m2_t t;}
void foo16 () {vuint8m2_t t;}
void foo17 () {vint8m4_t t;}
void foo18 () {vuint8m4_t t;}
void foo19 () {vint8m8_t t;}
void foo20 () {vuint8m8_t t;}
void foo21 () {vint16mf4_t t;} /* { dg-error {unknown type name 'vint16mf4_t'} } */
void foo22 () {vuint16mf4_t t;} /* { dg-error {unknown type name 'vuint16mf4_t'} } */
void foo23 () {vint16mf2_t t;}
void foo24 () {vuint16mf2_t t;}
void foo25 () {vint16m1_t t;}
void foo26 () {vuint16m1_t t;}
void foo27 () {vint16m2_t t;}
void foo28 () {vuint16m2_t t;}
void foo29 () {vint16m4_t t;}
void foo30 () {vuint16m4_t t;}
void foo31 () {vint16m8_t t;}
void foo32 () {vuint16m8_t t;}
void foo33 () {vint32mf2_t t;} /* { dg-error {unknown type name 'vint32mf2_t'} } */
void foo34 () {vuint32mf2_t t;} /* { dg-error {unknown type name 'vuint32mf2_t'} } */
void foo35 () {vint32m1_t t;}
void foo36 () {vuint32m1_t t;}
void foo37 () {vint32m2_t t;}
void foo38 () {vuint32m2_t t;}
void foo39 () {vint32m4_t t;}
void foo40 () {vuint32m4_t t;}
void foo41 () {vint32m8_t t;}
void foo42 () {vuint32m8_t t;}
void foo43 () {vint64m1_t t;} /* { dg-error {unknown type name 'vint64m1_t'} } */
void foo44 () {vuint64m1_t t;} /* { dg-error {unknown type name 'vuint64m1_t'} } */
void foo45 () {vint64m2_t t;} /* { dg-error {unknown type name 'vint64m2_t'} } */
void foo46 () {vuint64m2_t t;} /* { dg-error {unknown type name 'vuint64m2_t'} } */
void foo47 () {vint64m4_t t;} /* { dg-error {unknown type name 'vint64m4_t'} } */
void foo48 () {vuint64m4_t t;} /* { dg-error {unknown type name 'vuint64m4_t'} } */
void foo49 () {vint64m8_t t;} /* { dg-error {unknown type name 'vint64m8_t'} } */
void foo50 () {vuint64m8_t t;} /* { dg-error {unknown type name 'vuint64m8_t'} } */
void foo57 () {vfloat32mf2_t t;} /* { dg-error {unknown type name 'vfloat32mf2_t'} } */
void foo58 () {vfloat32m1_t t;} /* { dg-error {unknown type name 'vfloat32m1_t'} } */
void foo59 () {vfloat32m2_t t;} /* { dg-error {unknown type name 'vfloat32m2_t'} } */
void foo60 () {vfloat32m4_t t;} /* { dg-error {unknown type name 'vfloat32m4_t'} } */
void foo61 () {vfloat32m8_t t;} /* { dg-error {unknown type name 'vfloat32m8_t'} } */
void foo62 () {vfloat64m1_t t;} /* { dg-error {unknown type name 'vfloat64m1_t'} } */
void foo63 () {vfloat64m2_t t;} /* { dg-error {unknown type name 'vfloat64m2_t'} } */
void foo64 () {vfloat64m4_t t;} /* { dg-error {unknown type name 'vfloat64m4_t'} } */
void foo65 () {vfloat64m8_t t;} /* { dg-error {unknown type name 'vfloat64m8_t'} } */
