/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve32x -mabi=ilp32d" } */

#include "riscv_vector.h"

void foo0 () {vbool64_t t;}
void foo1 () {vbool32_t t;}
void foo2 () {vbool16_t t;}
void foo3 () {vbool8_t t;}
void foo4 () {vbool4_t t;}
void foo5 () {vbool2_t t;}
void foo6 () {vbool1_t t;}
void foo7 () {vint8mf8_t t;}
void foo8 () {vuint8mf8_t t;}
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
void foo21 () {vint16mf4_t t;}
void foo22 () {vuint16mf4_t t;}
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
void foo33 () {vint32mf2_t t;}
void foo34 () {vuint32mf2_t t;}
void foo35 () {vint32m1_t t;}
void foo36 () {vuint32m1_t t;}
void foo37 () {vint32m2_t t;}
void foo38 () {vuint32m2_t t;}
void foo39 () {vint32m4_t t;}
void foo40 () {vuint32m4_t t;}
void foo41 () {vint32m8_t t;}
void foo42 () {vuint32m8_t t;}
void foo43 () {vint64m1_t t;}
void foo44 () {vuint64m1_t t;}
void foo45 () {vint64m2_t t;}
void foo46 () {vuint64m2_t t;}
void foo47 () {vint64m4_t t;}
void foo48 () {vuint64m4_t t;}
void foo49 () {vint64m8_t t;}
void foo50 () {vuint64m8_t t;}

/* To support target attribute, the vfloat*m*x*_t need to be registered
   in advance.  We add type and args/return value check during the
   set current function but cannot cover below cases.  It is the same
   behavior compared to aarch64 sve.  */
void foo57 () {vfloat32mf2_t t;}
void foo58 () {vfloat32m1_t t;}
void foo59 () {vfloat32m2_t t;}
void foo60 () {vfloat32m4_t t;}
void foo61 () {vfloat32m8_t t;}
void foo62 () {vfloat64m1_t t;}
void foo63 () {vfloat64m2_t t;}
void foo64 () {vfloat64m4_t t;}
void foo65 () {vfloat64m8_t t;}

void new_foo0 (vbool64_t t)  { }     /* { dg-error {argument type 'vbool64_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo7 (vint8mf8_t t)  { }    /* { dg-error {argument type 'vint8mf8_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo8 (vuint8mf8_t t)  { }   /* { dg-error {argument type 'vuint8mf8_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo21 (vint16mf4_t t) { }   /* { dg-error {argument type 'vint16mf4_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo22 (vuint16mf4_t t) { }  /* { dg-error {argument type 'vuint16mf4_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo33 (vint32mf2_t t) { }   /* { dg-error {argument type 'vint32mf2_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo34 (vuint32mf2_t t) { }  /* { dg-error {argument type 'vuint32mf2_t' requires the minimal vector length '64' but '32' is given} } */
void new_foo43 (vint64m1_t t) { }    /* { dg-error {argument type 'vint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo44 (vuint64m1_t t) { }   /* { dg-error {argument type 'vuint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo45 (vint64m2_t t) { }    /* { dg-error {argument type 'vint64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo46 (vuint64m2_t t) { }   /* { dg-error {argument type 'vuint64m2_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo47 (vint64m4_t t) { }    /* { dg-error {argument type 'vint64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo48 (vuint64m4_t t) { }   /* { dg-error {argument type 'vuint64m4_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo49 (vint64m8_t t) { }    /* { dg-error {argument type 'vint64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo50 (vuint64m8_t t) { }   /* { dg-error {argument type 'vuint64m8_t' requires the zve64x, zve64f, zve64d or v ISA extension} } */
void new_foo57 (vfloat32mf2_t t) { } /* { dg-error {argument type 'vfloat32mf2_t' requires the zve32f, zve64f, zve64d or v ISA extension} } */
void new_foo58 (vfloat32m1_t t) { }  /* { dg-error {argument type 'vfloat32m1_t' requires the zve32f, zve64f, zve64d or v ISA extension} } */
void new_foo59 (vfloat32m2_t t) { }  /* { dg-error {argument type 'vfloat32m2_t' requires the zve32f, zve64f, zve64d or v ISA extension} } */
void new_foo60 (vfloat32m4_t t) { }  /* { dg-error {argument type 'vfloat32m4_t' requires the zve32f, zve64f, zve64d or v ISA extension} } */
void new_foo61 (vfloat32m8_t t) { }  /* { dg-error {argument type 'vfloat32m8_t' requires the zve32f, zve64f, zve64d or v ISA extension} } */
void new_foo62 (vfloat64m1_t t) { }  /* { dg-error {argument type 'vfloat64m1_t' requires the zve64d or v ISA extension} } */
void new_foo63 (vfloat64m2_t t) { }  /* { dg-error {argument type 'vfloat64m2_t' requires the zve64d or v ISA extension} } */
void new_foo64 (vfloat64m4_t t) { }  /* { dg-error {argument type 'vfloat64m4_t' requires the zve64d or v ISA extension} } */
void new_foo65 (vfloat64m8_t t) { }  /* { dg-error {argument type 'vfloat64m8_t' requires the zve64d or v ISA extension} } */
