/* PR target/96246 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpblendm\[bwdq\]\[\t ]" 6 } } */
/* { dg-final { scan-assembler-times "vblendmp\[sd\]\[\t ]" 3 } } */

typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v32qi __attribute__ ((vector_size (32)));
typedef char v16hi __attribute__ ((vector_size (32)));
typedef int v4si __attribute__((vector_size (16)));
typedef int v8si __attribute__((vector_size (32)));
typedef long long v4di __attribute__((vector_size (32)));
typedef float v4sf __attribute__((vector_size (16)));
typedef float v8sf __attribute__((vector_size (32)));
typedef double v4df __attribute__((vector_size (32)));

#define COMPILE_TEST(vtype, num)			\
  vtype							\
  __attribute__ ((noipa))				\
  foo_##vtype (vtype a, vtype b, vtype c, vtype d)	\
  {							\
    vtype e;						\
    for (int i = 0; i != num; i++)			\
      e[i] = a[i] > b[i] ? c[i] : d[i];			\
    return e;						\
  }

COMPILE_TEST (v16qi, 16);
COMPILE_TEST (v32qi, 32);
COMPILE_TEST (v16hi, 16);
COMPILE_TEST (v4si, 4);
COMPILE_TEST (v8si, 8);
COMPILE_TEST (v4sf, 4);
COMPILE_TEST (v8sf, 8);
COMPILE_TEST (v4di, 4);
COMPILE_TEST (v4df, 4);
