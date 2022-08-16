/* PR target/87767 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mavx512vl" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } }
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to2\\\}" 4 } }  */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to4\\\}" 8 } }  */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to8\\\}" 4 } }  */

typedef float v4sf  __attribute__ ((vector_size (16)));
typedef float v8sf  __attribute__ ((vector_size (32)));
typedef double v2df  __attribute__ ((vector_size (16)));
typedef double v4df  __attribute__ ((vector_size (32)));

#define CONSTANT 101;
#define FOO(VTYPE, OP_NAME, OP1, OP2)		\
VTYPE						\
 __attribute__ ((noipa))			\
 foo_##OP_NAME##_##VTYPE (VTYPE a, VTYPE b)	\
{						\
  return (OP1 a * b) OP2 CONSTANT;		\
}						\

FOO (v4sf, fma,, +);
FOO (v8sf, fma,, +);
FOO (v2df, fma,, +);
FOO (v4df, fma,, +);
FOO (v4sf, fms,, -);
FOO (v8sf, fms,, -);
FOO (v2df, fms,, -);
FOO (v4df, fms,, -);
FOO (v4sf, fnma, -, +);
FOO (v8sf, fnma, -, +);
FOO (v2df, fnma, -, +);
FOO (v4df, fnma, -, +);
FOO (v4sf, fnms, -, -);
FOO (v8sf, fnms, -, -);
FOO (v2df, fnms, -, -);
FOO (v4df, fnms, -, -);
