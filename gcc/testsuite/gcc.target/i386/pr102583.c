/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times {(?n)vcvtdq2ps[ \t]+32\(%.*%ymm} 1 } } */
/* { dg-final { scan-assembler-times {(?n)vcvtdq2ps[ \t]+16\(%.*%xmm} 1 } } */
/* { dg-final { scan-assembler-times {(?n)vmovq[ \t]+16\(%.*%xmm} 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not {(?n)vpermd[ \t]+.*%zmm} } } */

typedef int v16si __attribute__((vector_size(64)));
typedef float v8sf __attribute__((vector_size(32)));
typedef float v4sf __attribute__((vector_size(16)));
typedef float v2sf __attribute__((vector_size(8)));

v8sf part (v16si *srcp)
{
  v16si src = *srcp;
  return (v8sf) { (float)src[8], (float) src[9], (float)src[10], (float)src[11],
      (float)src[12], (float)src[13], (float)src[14], (float)src[15] };
}

v4sf part1 (v16si *srcp)
{
  v16si src = *srcp;
  return (v4sf) { (float)src[4], (float)src[5], (float)src[6], (float)src[7] };
}

v2sf part2 (v16si *srcp)
{
  v16si src = *srcp;
  return (v2sf) { (float)src[4], (float)src[5] };
}
