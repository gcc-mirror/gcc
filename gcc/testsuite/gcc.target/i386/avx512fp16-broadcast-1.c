/* PR target/87767 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mavx512dq" } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } }
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to8\\\}" 4 } }  */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to16\\\}" 4 } }  */
/* { dg-final { scan-assembler-times "\[^\n\]*\\\{1to32\\\}" 4 } }  */

typedef _Float16 v8hf  __attribute__ ((vector_size (16)));
typedef _Float16 v16hf  __attribute__ ((vector_size (32)));
typedef _Float16 v32hf  __attribute__ ((vector_size (64)));

#define CONSTANT 101;
#define FOO(VTYPE, OP_NAME, OP)                        \
VTYPE                                          \
 __attribute__ ((noipa))                       \
foo_##OP_NAME##_##VTYPE (VTYPE a)              \
{                                              \
  return a OP CONSTANT;                                \
}                                              \

FOO (v8hf, add, +);
FOO (v16hf, add, +);
FOO (v32hf, add, +);
FOO (v8hf, sub, -);
FOO (v16hf, sub, -);
FOO (v32hf, sub, -);
FOO (v8hf, mul, *);
FOO (v16hf, mul, *);
FOO (v32hf, mul, *);
FOO (v8hf, div, /);
FOO (v16hf, div, /);
FOO (v32hf, div, /);
