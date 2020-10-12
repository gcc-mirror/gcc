/* PR target/87767 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -mno-avx512f" } */
/* { dg-final { scan-assembler-not "\\\{1to\[248\]\\\}" } }  */
/* { dg-final { scan-assembler-not "\\\{1to16\\\}" } }  */

typedef int v4si  __attribute__ ((vector_size (16)));
typedef int v8si  __attribute__ ((vector_size (32)));
typedef long long v2di  __attribute__ ((vector_size (16)));
typedef long long v4di  __attribute__ ((vector_size (32)));
typedef float v4sf  __attribute__ ((vector_size (16)));
typedef float v8sf  __attribute__ ((vector_size (32)));
typedef double v2df  __attribute__ ((vector_size (16)));
typedef double v4df  __attribute__ ((vector_size (32)));

#define FOO(VTYPE, OP_NAME, OP)			\
VTYPE						\
 __attribute__ ((noipa))			\
foo_##OP_NAME##_##VTYPE (VTYPE a)		\
{						\
  return a OP 101;				\
}						\

FOO (v4si, add, +);
FOO (v8si, add, +);
FOO (v2di, add, +);
FOO (v4di, add, +);
FOO (v4sf, add, +);
FOO (v8sf, add, +);
FOO (v2df, add, +);
FOO (v4df, add, +);

FOO (v4si, mul, *);
FOO (v8si, mul, *);
FOO (v2di, mul, *);
FOO (v4di, mul, *);
FOO (v4sf, mul, *);
FOO (v8sf, mul, *);
FOO (v2df, mul, *);
FOO (v4df, mul, *);
