/* { dg-do compile } */
/* { dg-options "-msse4.1 -O2 -mno-avx512f" } */
/* { dg-final { scan-assembler-not "packuswb" } }  */
/* { dg-final { scan-assembler-times "pshufb" 4 { target { ! ia32 } } } }  */
/* { dg-final { scan-assembler-times "pshufb" 6 { target  ia32 } } }  */

typedef unsigned char v8uqi __attribute__((vector_size(8)));
typedef  char v8qi __attribute__((vector_size(8)));
typedef unsigned char v4uqi __attribute__((vector_size(4)));
typedef  char v4qi __attribute__((vector_size(4)));

v8qi
foo (v8qi a)
{
  return a >> 5;
}

v8uqi
foo1 (v8uqi a)
{
  return a >> 5;
}

v4qi
foo2 (v4qi a)
{
  return a >> 5;
}

v4uqi
foo3 (v4uqi a)
{
  return a >> 5;
}

