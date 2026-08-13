/* { dg-do compile } */
/* { dg-options "-O -msse2" } */

typedef int  v2si __attribute__((vector_size (8)));
typedef unsigned int  v2usi __attribute__((vector_size (8)));
typedef long long v2di __attribute__((vector_size (16)));

v2si
f1 (v2si a, v2si b)
{

  v2di z = __builtin_convertvector (a, v2di);
  return __builtin_convertvector (z, v2si);
}

v2usi
f2 (v2si a, v2si b)
{

  v2di z = __builtin_convertvector (a, v2di);
  return __builtin_convertvector (z, v2usi);
}

/* { dg-final { scan-assembler-not "xmm" { target { ! ia32 } } } } */
