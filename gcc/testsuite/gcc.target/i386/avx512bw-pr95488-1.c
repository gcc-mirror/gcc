/* PR target/95488  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mprefer-vector-width=512" }  */
/* { dg-final { scan-assembler-times "vpmovzxbw" 4 } } */
/* { dg-final { scan-assembler-times "vpmullw\[^\n\]*zmm" 2 } } */
/* { dg-final { scan-assembler-times "vpmovwb" 2 } } */

typedef char v32qi __attribute__ ((vector_size (32)));
typedef unsigned char v32uqi __attribute__ ((vector_size (32)));

__attribute__((noipa)) v32qi
mul_512 (v32qi a, v32qi b)
{
  return  a * b;
}

__attribute__((noipa)) v32uqi
umul_512 (v32uqi a, v32uqi b)
{
  return  a * b;
}
