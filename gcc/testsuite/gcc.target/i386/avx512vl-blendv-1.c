/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times {pblendvb[\t ]*%xmm} 1 } } */
/* { dg-final { scan-assembler-times {pblendvb[\t ]*%ymm} 1 } } */
/* { dg-final { scan-assembler-times {blendvps[\t ]*%xmm} 1 } } */
/* { dg-final { scan-assembler-times {blendvps[\t ]*%ymm} 1 } } */
/* { dg-final { scan-assembler-times {blendvpd[\t ]*%xmm} 1 } } */
/* { dg-final { scan-assembler-times {blendvpd[\t ]*%ymm} 1 } } */

typedef float v4sf __attribute__ ((vector_size (16)));
typedef float v8sf __attribute__ ((vector_size (32)));
typedef double v2df __attribute__ ((vector_size (16)));
typedef double v4df __attribute__ ((vector_size (32)));
typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v32qi __attribute__ ((vector_size (32)));

v4sf
foo (v4sf a, v4sf b, v4sf c)
{
  return __builtin_ia32_blendvps (a, b, c);
}

v8sf
foo2 (v8sf a, v8sf b, v8sf c)
{
  return __builtin_ia32_blendvps256 (a, b, c);
}

v2df
foo3 (v2df a, v2df b, v2df c)
{
  return __builtin_ia32_blendvpd (a, b, c);
}

v4df
foo4 (v4df a, v4df b, v4df c)
{
  return __builtin_ia32_blendvpd256 (a, b, c);
}

v16qi
foo5 (v16qi a, v16qi b, v16qi c)
{
  return __builtin_ia32_pblendvb128 (a, b, c);
}

v32qi
foo6 (v32qi a, v32qi b, v32qi c)
{
  return __builtin_ia32_pblendvb256 (a, b, c);
}
