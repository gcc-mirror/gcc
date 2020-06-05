/* { dg-do compile } */
/* { dg-options "-O2 -ftree-slp-vectorize -march=armv8.2-a+sve -msve-vector-bits=256" } */

typedef short __attribute__((vector_size (8))) v4hi;

typedef union U4HI { v4hi v; short a[4]; } u4hi;

short b[4];

void pass_v4hi (v4hi v)
{
    int i;
    u4hi u;
    u.v = v;
    for (i = 0; i < 4; i++)
      b[i] = u.a[i];
};

/* { dg-final { scan-assembler-not "ptrue" } } */
