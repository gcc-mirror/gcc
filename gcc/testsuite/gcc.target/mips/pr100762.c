/* PR target/100762
   This was triggering an ICE in mips_expand_vector_init when compiled with
   -mmsa -mloongson-mmi. */

/* { dg-do compile } */
/* { dg-options "-mmsa -mloongson-mmi" } */

typedef __INT32_TYPE__ int32_t;
typedef int32_t i32x2 __attribute__((__vector_size__(8)));

i32x2 cmp(i32x2 a, i32x2 b) {
  return a >= b;
}

i32x2 shift(i32x2 a, i32x2 b) {
  return a >> b;
}

i32x2 mul(i32x2 a, i32x2 b) {
  return a * b;
}

i32x2 div(i32x2 a, i32x2 b) {
  return a / b;
}
