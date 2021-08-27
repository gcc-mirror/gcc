/* PR target/95524 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx" } */
/* { dg-final { scan-assembler-times "pand\[^\n\]*%xmm" 3 { xfail *-*-* } } } */
typedef char v16qi  __attribute__ ((vector_size (16)));
typedef unsigned char v16uqi  __attribute__ ((vector_size (16)));

__attribute__((noipa)) v16qi
foo_ashiftrt_128 (v16qi a)
{
  return a >> 2;
}
/* { dg-final { scan-assembler-times "psraw\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "pxor\[^\n\]*%xmm" 1 } } */
/* { dg-final { scan-assembler-times "psubb\[^\n\]*%xmm" 1 } } */

__attribute__((noipa)) v16qi
foo_ashift_128 (v16qi a)
{
  return a << 7;
}

/* { dg-final { scan-assembler-times "psllw\[^\n\]*%xmm" 1 { xfail *-*-* } } } */

__attribute__((noipa)) v16uqi
foo_lshiftrt_128 (v16uqi a)
{
  return a >> 2;
}

/* { dg-final { scan-assembler-times "psrlw\[^\n\]*%xmm" 1 } }  */
