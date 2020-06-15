/* PR target/95524 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-final { scan-assembler-times "vpand\[^\n\]*%zmm" 3 } }  */
typedef char v64qi  __attribute__ ((vector_size (64)));
typedef unsigned char v64uqi  __attribute__ ((vector_size (64)));

__attribute__((noipa)) v64qi
foo_ashiftrt_512 (v64qi a)
{
  return a >> 2;
}
/* { dg-final { scan-assembler-times "vpsraw\[^\n\]*%zmm" 1 } } */
/* { dg-final { scan-assembler-times "vpxor\[^\n\]*%zmm" 1 } } */
/* { dg-final { scan-assembler-times "vpsubb\[^\n\]*%zmm" 1 } } */

__attribute__((noipa)) v64qi
foo_ashift_512 (v64qi a)
{
  return a << 7;
}

/* { dg-final { scan-assembler-times "vpsllw\[^\n\]*%zmm" 1 } }  */

__attribute__((noipa)) v64uqi
foo_lshiftrt_512 (v64uqi a)
{
  return a >> 2;
}

/* { dg-final { scan-assembler-times "vpsrlw\[^\n\]*%zmm" 1 } }  */
