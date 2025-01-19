/* { dg-do compile } */
/* { dg-options "-O2 -fschedule-insns" } */

typedef int __attribute__((vector_size(8))) vec;

vec
load_long(vec *v) {
  return v[110] + v[111] + v[112] + v[113];
}

/* { dg-final { scan-assembler {add\tx[0-9]+, x[0-9]+, 880} } } */
/* { dg-final { scan-assembler {ldp\td[0-9]+, d[0-9]+, \[x[0-9]+\]} } } */
/* { dg-final { scan-assembler {ldp\td[0-9]+, d[0-9]+, \[x[0-9]+, 16\]} } } */
/* { dg-final { scan-assembler-not "ldr\t" } } */
