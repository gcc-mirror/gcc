/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvbb -mabi=lp64d -fno-vect-cost-model -mrvv-vector-bits=zvl" } */

typedef int a;
void *b;
a c;
void d() {
  a e = c, f =0;
  short *g = b;
  for (; f < e; f++)
    *(g + f) = (255 & (*(g + f) >> 8)) | *(g + f) << 8;
}

