/* PR target/87767 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mavx512vl" } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } }
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to2\\\}" 4 { target ia32 } } } */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to4\\\}" 4 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%(?:r|e)\[^\n\]*, %xmm\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%(?:r|e)\[^\n\]*, %ymm\[0-9\]+" 4 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+%r\[^\n\]*, %xmm\[0-9\]+" 4 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+%r\[^\n\]*, %ymm\[0-9\]+" 4 { target { ! ia32 } } } } */

typedef int v4si  __attribute__ ((vector_size (16)));
typedef int v8si  __attribute__ ((vector_size (32)));
typedef long long v2di  __attribute__ ((vector_size (16)));
typedef long long v4di  __attribute__ ((vector_size (32)));

#define CONSTANT 101;
#define FOO(VTYPE, OP_NAME, OP1, OP2)		\
VTYPE						\
 __attribute__ ((noipa))			\
 foo_##OP_NAME##_##VTYPE (VTYPE a)		\
{						\
  return (OP1 a) OP2 CONSTANT;			\
}						\

FOO (v4si, andnot, ~, &);
FOO (v8si, andnot, ~, &);
FOO (v2di, andnot, ~, &);
FOO (v4di, andnot, ~, &);
FOO (v4si, and,, &);
FOO (v8si, and,, &);
FOO (v2di, and,, &);
FOO (v4di, and,, &);
FOO (v4si, or,, |);
FOO (v8si, or,, |);
FOO (v2di, or,, |);
FOO (v4di, or,, |);
FOO (v4si, xor,, ^);
FOO (v8si, xor,, ^);
FOO (v2di, xor,, ^);
FOO (v4di, xor,, ^);
