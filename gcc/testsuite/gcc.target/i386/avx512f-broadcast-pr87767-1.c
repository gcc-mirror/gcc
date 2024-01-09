/* PR target/87767 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mavx512dq" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } }
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to8\\\}" 2 } } */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to16\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%(?:r|e)\[^\n\]*, %zmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+%r\[^\n\]*, %zmm\[0-9\]+" 3 { target { ! ia32 } } } } */

typedef int v16si  __attribute__ ((vector_size (64)));
typedef long long v8di  __attribute__ ((vector_size (64)));
typedef float v16sf  __attribute__ ((vector_size (64)));
typedef double v8df  __attribute__ ((vector_size (64)));

#define CONSTANT 101;
#define FOO(VTYPE, OP_NAME, OP)			\
VTYPE						\
 __attribute__ ((noipa))			\
foo_##OP_NAME##_##VTYPE (VTYPE a)		\
{						\
  return a OP CONSTANT;				\
}						\

FOO (v16si, add, +);
FOO (v8di, add, +);
FOO (v16sf, add, +);
FOO (v8df, add, +);
FOO (v16si, sub, -);
FOO (v8di, sub, -);
FOO (v16si, mul, *);
FOO (v8di, mul, *);
FOO (v16sf, mul, *);
FOO (v8df, mul, *);
