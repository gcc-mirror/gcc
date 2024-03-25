/* PR target/87767 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } }
/* { dg-final { scan-assembler-not "\[^\n\]*\\\{1to8\\\}" { target ia32 } } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%(?:r|e)\[^\n\]*, %zmm\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+%r\[^\n\]*, %zmm\[0-9\]+" 4 { target { ! ia32 } } } } */

typedef int v16si  __attribute__ ((vector_size (64)));
typedef long long v8di  __attribute__ ((vector_size (64)));

#define CONSTANT 101;
#define FOO(VTYPE, OP_NAME, OP1, OP2)		\
VTYPE						\
 __attribute__ ((noipa))			\
 foo_##OP_NAME##_##VTYPE (VTYPE a)		\
{						\
  return (OP1 a) OP2 CONSTANT;			\
}						\

FOO (v16si, andnot, ~, &);
FOO (v8di, andnot, ~, &);
FOO (v16si, and,, &);
FOO (v8di, and,, &);
FOO (v16si, or,, |);
FOO (v8di, or,, |);
FOO (v16si, xor,, ^);
FOO (v8di, xor,, ^);
