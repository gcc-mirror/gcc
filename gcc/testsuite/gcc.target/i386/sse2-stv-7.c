/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse2 -mno-sse4.1 -mtune=generic" } */

typedef long long v2di __attribute__ ((__vector_size__ (16)));

long long foo(v2di x)
{
  return x[0];
}

long long ext();
v2di mem;

void bar()
{
  long long x = ext();
  mem = (v2di){x,0};
}

/* { dg-final { scan-assembler "pshufd" } } */
/* { dg-final { scan-assembler "punpckldq" } } */
/* { dg-final { scan-assembler-not "movq" } } */
