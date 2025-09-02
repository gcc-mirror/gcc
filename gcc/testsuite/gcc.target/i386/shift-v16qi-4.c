/* { dg-do compile } */
/* { dg-options "-mgfni -mavx512vl -mavx512bw -mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpcmpgtb" 1 } } */

typedef char v16qi __attribute__((vector_size(16)));

v16qi
foo (v16qi a)
{
  return a >> 7;
}
