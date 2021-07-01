/* PR target/87767 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mavx512vl -mavx512dq" } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } }
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to2\\\}" 2 { target { ! ia32 } } } }  */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to4\\\}" 4 { target { ! ia32 } } } }  */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to2\\\}" 5 { target ia32 } } } */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to4\\\}" 7 { target ia32 } } } */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%(?:r|e)\[^\n\]*, %xmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%(?:r|e)\[^\n\]*, %ymm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+%r\[^\n\]*, %xmm\[0-9\]+" 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+%r\[^\n\]*, %ymm\[0-9\]+" 3 { target { ! ia32 } } } } */

typedef int v4si  __attribute__ ((vector_size (16)));
typedef int v8si  __attribute__ ((vector_size (32)));
typedef long long v2di  __attribute__ ((vector_size (16)));
typedef long long v4di  __attribute__ ((vector_size (32)));
typedef float v4sf  __attribute__ ((vector_size (16)));
typedef float v8sf  __attribute__ ((vector_size (32)));
typedef double v2df  __attribute__ ((vector_size (16)));
typedef double v4df  __attribute__ ((vector_size (32)));

#define CONSTANT 101;
#define FOO(VTYPE, OP_NAME, OP)			\
VTYPE						\
 __attribute__ ((noipa))			\
foo_##OP_NAME##_##VTYPE (VTYPE a)		\
{						\
  return a OP CONSTANT;				\
}						\

FOO (v4si, add, +);
FOO (v8si, add, +);
FOO (v2di, add, +);
FOO (v4di, add, +);
FOO (v4si, sub, -);
FOO (v8si, sub, -);
FOO (v2di, sub, -);
FOO (v4di, sub, -);
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
