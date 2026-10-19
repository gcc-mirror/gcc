/* PR target/121688 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-f16c -mavx512f -mno-avx512vl -mno-avx512fp16 -masm=att" } */
/* { dg-final { scan-assembler-times "\tvcvtps2ph\t\\\$4, %zmm\[0-9]\+, %ymm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t%ymm\[0-9]\+, %zmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %zmm\[0-9]\+" } } */

typedef _Float16 v16hf __attribute__((vector_size (32)));
typedef float v16sf __attribute__((vector_size (64)));

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
