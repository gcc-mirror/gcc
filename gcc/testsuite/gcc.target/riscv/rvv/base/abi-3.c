/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc_zve64x -mabi=ilp32d" } */

void foo0 () {__rvv_bool64_t t;}
void foo1 () {__rvv_bool32_t t;}
void foo2 () {__rvv_bool16_t t;}
void foo3 () {__rvv_bool8_t t;}
void foo4 () {__rvv_bool4_t t;}
void foo5 () {__rvv_bool2_t t;}
void foo6 () {__rvv_bool1_t t;}
void foo7 () {__rvv_int8mf8_t t;}
void foo8 () {__rvv_uint8mf8_t t;}
void foo9 () {__rvv_int8mf4_t t;}
void foo10 () {__rvv_uint8mf4_t t;}
void foo11 () {__rvv_int8mf2_t t;}
void foo12 () {__rvv_uint8mf2_t t;}
void foo13 () {__rvv_int8m1_t t;}
void foo14 () {__rvv_uint8m1_t t;}
void foo15 () {__rvv_int8m2_t t;}
void foo16 () {__rvv_uint8m2_t t;}
void foo17 () {__rvv_int8m4_t t;}
void foo18 () {__rvv_uint8m4_t t;}
void foo19 () {__rvv_int8m8_t t;}
void foo20 () {__rvv_uint8m8_t t;}
void foo21 () {__rvv_int16mf4_t t;}
void foo22 () {__rvv_uint16mf4_t t;}
void foo23 () {__rvv_int16mf2_t t;}
void foo24 () {__rvv_uint16mf2_t t;}
void foo25 () {__rvv_int16m1_t t;}
void foo26 () {__rvv_uint16m1_t t;}
void foo27 () {__rvv_int16m2_t t;}
void foo28 () {__rvv_uint16m2_t t;}
void foo29 () {__rvv_int16m4_t t;}
void foo30 () {__rvv_uint16m4_t t;}
void foo31 () {__rvv_int16m8_t t;}
void foo32 () {__rvv_uint16m8_t t;}
void foo33 () {__rvv_int32mf2_t t;}
void foo34 () {__rvv_uint32mf2_t t;}
void foo35 () {__rvv_int32m1_t t;}
void foo36 () {__rvv_uint32m1_t t;}
void foo37 () {__rvv_int32m2_t t;}
void foo38 () {__rvv_uint32m2_t t;}
void foo39 () {__rvv_int32m4_t t;}
void foo40 () {__rvv_uint32m4_t t;}
void foo41 () {__rvv_int32m8_t t;}
void foo42 () {__rvv_uint32m8_t t;}
void foo43 () {__rvv_int64m1_t t;}
void foo44 () {__rvv_uint64m1_t t;}
void foo45 () {__rvv_int64m2_t t;}
void foo46 () {__rvv_uint64m2_t t;}
void foo47 () {__rvv_int64m4_t t;}
void foo48 () {__rvv_uint64m4_t t;}
void foo49 () {__rvv_int64m8_t t;}
void foo50 () {__rvv_uint64m8_t t;}
void foo57 () {__rvv_float32mf2_t t;} /* { dg-error {unknown type name '__rvv_float32mf2_t'} } */
void foo58 () {__rvv_float32m1_t t;} /* { dg-error {unknown type name '__rvv_float32m1_t'} } */
void foo59 () {__rvv_float32m2_t t;} /* { dg-error {unknown type name '__rvv_float32m2_t'} } */
void foo60 () {__rvv_float32m4_t t;} /* { dg-error {unknown type name '__rvv_float32m4_t'} } */
void foo61 () {__rvv_float32m8_t t;} /* { dg-error {unknown type name '__rvv_float32m8_t'} } */
void foo62 () {__rvv_float64m1_t t;} /* { dg-error {unknown type name '__rvv_float64m1_t'} } */
void foo63 () {__rvv_float64m2_t t;} /* { dg-error {unknown type name '__rvv_float64m2_t'} } */
void foo64 () {__rvv_float64m4_t t;} /* { dg-error {unknown type name '__rvv_float64m4_t'} } */
void foo65 () {__rvv_float64m8_t t;} /* { dg-error {unknown type name '__rvv_float64m8_t'} } */
