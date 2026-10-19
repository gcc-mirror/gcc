/* PR target/121688 */
/* { dg-do compile } */
/* { dg-options "-O2 -mf16c -mno-avx512f -masm=att" } */
/* { dg-final { scan-assembler-times "\tvcvtps2ph\t\\\$4, %xmm\[0-9]\+, %xmm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler-times "\tvcvtps2ph\t\\\$4, %ymm\[0-9]\+, %xmm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler-not "\tvcvtps2ph\t\\\$4, %zmm\[0-9]\+, %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t%xmm\[0-9]\+, %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t%xmm\[0-9]\+, %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler-not "\tvcvtph2ps\t%ymm\[0-9]\+, %zmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler-not "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %zmm\[0-9]\+" } } */

typedef _Float16 v4hf __attribute__((vector_size (8)));
typedef _Float16 v8hf __attribute__((vector_size (16)));
typedef _Float16 v16hf __attribute__((vector_size (32)));
typedef float v4sf __attribute__((vector_size (16)));
typedef float v8sf __attribute__((vector_size (32)));
typedef float v16sf __attribute__((vector_size (64)));

v4sf
foo (v4hf x)
{
  return __builtin_convertvector (x, v4sf);
}

v4hf
bar (v4sf x)
{
  return __builtin_convertvector (x, v4hf);
}

v8sf
baz (v8hf x)
{
  return __builtin_convertvector (x, v8sf);
}

v8hf
qux (v8sf x)
{
  return __builtin_convertvector (x, v8hf);
}

#ifdef __AVX512VL__
v16sf
corge (v16hf x)
{
  return __builtin_convertvector (x, v16sf);
}

v16hf
garply (v16sf x)
{
  return __builtin_convertvector (x, v16hf);
}
#endif

v4sf
foo2 (v4hf *x)
{
  return __builtin_convertvector (*x, v4sf);
}

v4hf
bar2 (v4sf *x)
{
  return __builtin_convertvector (*x, v4hf);
}

v8sf
baz2 (v8hf *x)
{
  return __builtin_convertvector (*x, v8sf);
}

v8hf
qux2 (v8sf *x)
{
  return __builtin_convertvector (*x, v8hf);
}

#ifdef __AVX512VL__
v16sf
corge2 (v16hf *x)
{
  return __builtin_convertvector (*x, v16sf);
}

v16hf
garply2 (v16sf *x)
{
  return __builtin_convertvector (*x, v16hf);
}
#endif
