/* { dg-options "-O2 -fno-vect-cost-model" } */

typedef unsigned int v4si __attribute__((vector_size(16)));

unsigned int
foo (v4si *ptr)
{
  v4si x = __builtin_shufflevector (ptr[0], ptr[2], 0, 7, 1, 5);
  v4si y = x ^ ptr[4];
  return (y[0] + y[1] + y[2] + y[3]) >> 1;
}

/* { dg-final { scan-assembler {\taddv\t} } } */
/* { dg-final { scan-assembler-not {\tmov\t} } } */
