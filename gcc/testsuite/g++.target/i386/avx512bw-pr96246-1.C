/* PR target/96246 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=c++14 -mavx512bw" } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-final { scan-assembler-times "vpblendm\[bwdq\]\[\t \]" 4 } } */
/* { dg-final { scan-assembler-times "vblendmp\[sd\]\[\t \]" 2 } } */

typedef char v64qi __attribute__((vector_size (64)));
typedef short v32hi __attribute__((vector_size (64)));
typedef int v16si __attribute__((vector_size (64)));
typedef long long v8di __attribute__((vector_size (64)));
typedef float v16sf __attribute__((vector_size (64)));
typedef double v8df __attribute__((vector_size (64)));

#define COMPILE_TEST(vtype, num)			\
  vtype							\
  __attribute__ ((noipa))				\
  foo_##vtype (vtype a, vtype b, vtype c, vtype d)	\
  {							\
    return a > b ? c : d;				\
  }

COMPILE_TEST (v64qi, 64);
COMPILE_TEST (v32hi, 32);
COMPILE_TEST (v16si, 16);
COMPILE_TEST (v8di, 8);
COMPILE_TEST (v16sf, 16);
COMPILE_TEST (v8df, 8);
