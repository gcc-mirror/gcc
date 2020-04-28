/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -ftree-slp-vectorize -march=armv8.2-a+sve -msve-vector-bits=256" } */

typedef short __attribute__((vector_size (8))) v4hi;

typedef union U4HI { v4hi v; short a[4]; } u4hi;

short a[4];

void pass_v4hi (v4hi v) {
    int j;
    u4hi u;
    u.v = v;
    for (j = 0; j < 4; j++)
      a[j] = u.a[j];
};
