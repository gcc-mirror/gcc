/* PR target/101922
   This was triggering an assembler error with -O3 -mmsa -mloongson-mmi. */

/* { dg-do assemble } */
/* { dg-options "-mmsa -mloongson-mmi" } */

typedef __INT8_TYPE__ i8;
typedef __INT32_TYPE__ i32;

i8 d[16];

i32 f(i32 x) {
  int i;
  for (i = 0; i < 16; i++) {
    i32 t = (i32) d[i] >> 31;
    x &= t;
  }
  return x;
}
