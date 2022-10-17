/* PR target/95524 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -mno-avx512f" } */
/* { dg-final { scan-assembler-times "vpand\[^\n\]*%ymm" 3 } }  */
typedef char v32qi  __attribute__ ((vector_size (32)));
typedef unsigned char v32uqi  __attribute__ ((vector_size (32)));

__attribute__((noipa)) v32qi
foo_ashiftrt_256 (v32qi a)
{
  return a >> 2;
}
/* { dg-final { scan-assembler-times "vpsraw\[^\n\]*%ymm" 1 } } */
/* { dg-final { scan-assembler-times "vpxor\[^\n\]*%ymm" 1 } } */
/* { dg-final { scan-assembler-times "vpsubb\[^\n\]*%ymm" 1 } } */

__attribute__((noipa)) v32qi
foo_ashift_256 (v32qi a)
{
  return a << 7;
}

/* { dg-final { scan-assembler-times "vpsllw\[^\n\]*%ymm" 1 } }  */

__attribute__((noipa)) v32uqi
foo_lshiftrt_256 (v32uqi a)
{
  return a >> 2;
}

/* { dg-final { scan-assembler-times "vpsrlw\[^\n\]*%ymm" 1 } }  */
