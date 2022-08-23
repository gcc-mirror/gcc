/* PR target/87767 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } }
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to8\\\}" 4 } }  */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to16\\\}" 4 } }  */

typedef float v4sf  __attribute__ ((vector_size (16)));
typedef float v8sf  __attribute__ ((vector_size (32)));
typedef float v16sf  __attribute__ ((vector_size (64)));
typedef double v2df  __attribute__ ((vector_size (16)));
typedef double v4df  __attribute__ ((vector_size (32)));
typedef double v8df  __attribute__ ((vector_size (64)));

#define CONSTANT 101;
#define FOO(VTYPE, OP_NAME, OP1, OP2)		\
VTYPE						\
 __attribute__ ((noipa))			\
 foo_##OP_NAME##_##VTYPE (VTYPE a, VTYPE b)	\
{						\
  return (OP1 a * b) OP2 CONSTANT;		\
}						\

FOO (v16sf, fma,, +);
FOO (v8df, fma,, +);
FOO (v16sf, fms,, -);
FOO (v8df, fms,, -);
FOO (v16sf, fnma, -, +);
FOO (v8df, fnma, -, +);
FOO (v16sf, fnms, -, -);
FOO (v8df, fnms, -, -);
