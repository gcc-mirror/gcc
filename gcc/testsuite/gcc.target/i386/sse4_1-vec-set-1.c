/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse4.1 -O2" } */
/* { dg-final { scan-assembler-times {(?n)v?pcmpeq[bwd]} 4 } } */
/* { dg-final { scan-assembler-times {(?n)v?p?blendv} 4 } } */

typedef char v8qi __attribute__ ((vector_size (8)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef int v2si __attribute__ ((vector_size (8)));
typedef float v2sf __attribute__ ((vector_size (8)));

#define FOO(VTYPE, TYPE)			\
  VTYPE						\
  __attribute__ ((noipa))			\
  foo_##VTYPE (VTYPE a, TYPE b, unsigned int c)	\
  {						\
    a[c] = b;					\
    return a;					\
  }						\

FOO (v8qi, char);

FOO (v4hi, short);

FOO (v2si, int);

FOO (v2sf, float);
