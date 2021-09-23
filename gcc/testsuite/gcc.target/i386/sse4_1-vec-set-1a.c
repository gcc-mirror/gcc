/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse4.1 -O2" } */
/* { dg-final { scan-assembler-times {(?n)v?pcmpeq[bwd]} 2 } } */
/* { dg-final { scan-assembler-times {(?n)v?p?blendv} 2 } } */

typedef char v4qi __attribute__ ((vector_size (4)));
typedef short v2hi __attribute__ ((vector_size (4)));

#define FOO(VTYPE, TYPE)			\
  VTYPE						\
  __attribute__ ((noipa))			\
  foo_##VTYPE (VTYPE a, TYPE b, unsigned int c)	\
  {						\
    a[c] = b;					\
    return a;					\
  }						\

FOO (v4qi, char);

FOO (v2hi, short);
