/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-final { scan-assembler-times "pandn\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-not "vpternlogq\[ \\t\]" } } */

typedef unsigned long long vec __attribute__((vector_size (16)));
vec g;
vec f1 (vec a, vec b)
{
  return ~a&b;
}
vec f2 (vec a, vec b)
{
  return ~g&b;
}

