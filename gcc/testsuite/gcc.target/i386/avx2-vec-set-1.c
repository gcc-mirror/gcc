/* { dg-do compile } */
/* { dg-options "-mavx2 -O2 -mno-avx512f" } */
/* { dg-final { scan-assembler-times {(?n)vpcmpeq[bwdq]} 12 } } */
/* { dg-final { scan-assembler-times {(?n)vp?blendv} 12 } } */

typedef char v32qi __attribute__ ((vector_size (32)));
typedef char v16qi __attribute__ ((vector_size (16)));

typedef short v16hi __attribute__ ((vector_size (32)));
typedef short v8hi __attribute__ ((vector_size (16)));

typedef int v8si __attribute__ ((vector_size (32)));
typedef int v4si __attribute__ ((vector_size (16)));

typedef long long v4di __attribute__ ((vector_size (32)));
typedef long long v2di __attribute__ ((vector_size (16)));

typedef float v8sf __attribute__ ((vector_size (32)));
typedef float v4sf __attribute__ ((vector_size (16)));

typedef double v4df __attribute__ ((vector_size (32)));
typedef double v2df __attribute__ ((vector_size (16)));

#define FOO(VTYPE, TYPE)			\
  VTYPE						\
  __attribute__ ((noipa))			\
  foo_##VTYPE (VTYPE a, TYPE b, unsigned int c)	\
  {						\
    a[c] = b;					\
    return a;					\
  }						\

FOO (v16qi, char);
FOO (v32qi, char);

FOO (v8hi, short);
FOO (v16hi, short);

FOO (v4si, int);
FOO (v8si, int);

FOO (v2di, long long);
FOO (v4di, long long);

FOO (v4sf, float);
FOO (v8sf, float);

FOO (v2df, double);
FOO (v4df, double);
