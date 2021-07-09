/* PR target/100761
   This was triggering an ICE in mips_expand_vec_unpack when compiled with
   -mmsa -mloongson-mmi. */

/* { dg-do compile } */
/* { dg-options "-mmsa -mloongson-mmi" } */

typedef __INT8_TYPE__ int8_t;
typedef __INT16_TYPE__ int16_t;
typedef int8_t i8x8 __attribute__((__vector_size__(8)));
typedef int16_t i16x8 __attribute__((__vector_size__(16)));

i8x8 a;

void f() {
  i16x8 b = __builtin_convertvector (a, i16x8);
}
