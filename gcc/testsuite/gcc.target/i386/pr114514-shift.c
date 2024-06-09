/* { dg-do compile  } */
/* { dg-options "-mavx512vl -mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "vpxor" 4 } } */
/* { dg-final { scan-assembler-times "vpcmpgtb" 4 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpcmpgtb" 5 { target  ia32 } } } */
/* { dg-final { scan-assembler-times "vpmovb2m" 1 } } */
/* { dg-final { scan-assembler-times "vpmovm2b" 1 } } */


typedef char v16qi __attribute__((vector_size(16)));
typedef char v32qi __attribute__((vector_size(32)));
typedef char v64qi __attribute__((vector_size(64)));
typedef char v8qi __attribute__((vector_size(8)));
typedef char v4qi __attribute__((vector_size(4)));

v4qi
__attribute__((noipa))
foo1 (v4qi a)
{
  return a >> 7;
}

v8qi
__attribute__((noipa))
foo2 (v8qi a)
{
  return a >> 7;
}

v16qi
__attribute__((noipa))
foo3 (v16qi a)
{
  return a >> 7;
}

v32qi
__attribute__((noipa))
foo4 (v32qi a)
{
  return a >> 7;
}

v64qi
__attribute__((noipa))
foo5 (v64qi a)
{
  return a >> 7;
}
