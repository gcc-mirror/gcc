/* PR target/pr95488  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" }  */
/* { dg-final { scan-assembler-times "vpmovzxbw" 8 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmullw\[^\n\]*ymm" 2 } } */
/* { dg-final { scan-assembler-times "vpmullw\[^\n\]*xmm" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovwb" 4 { target { ! ia32 } } } } */

typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v8qi __attribute__ ((vector_size (8)));
typedef unsigned char v16uqi __attribute__ ((vector_size (16)));
typedef unsigned char v8uqi __attribute__ ((vector_size (8)));

__attribute__((noipa)) v8qi
mul_128 (v8qi a, v8qi b)
{
  return  a * b;
}

__attribute__((noipa)) v16qi
mul_256 (v16qi a, v16qi b)
{
  return  a * b;
}

__attribute__((noipa)) v8uqi
umul_128 (v8uqi a, v8uqi b)
{
  return  a * b;
}

__attribute__((noipa)) v16uqi
umul_256 (v16uqi a, v16uqi b)
{
  return  a * b;
}
