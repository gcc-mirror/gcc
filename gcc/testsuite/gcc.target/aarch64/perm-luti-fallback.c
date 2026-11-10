/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a+lut" } */

typedef unsigned char v16qi __attribute__((vector_size (16)));

v16qi
non_immediate_indices (v16qi x)
{
  return __builtin_shufflevector (x, x, 2, 0, 3, 1, 1, 3, 0, 2,
				 3, 2, 0, 1, 1, 0, 2, 3);
}

/* Packing the indices would still need a load.  Keep the ordinary TBL.  */
/* { dg-final { scan-assembler-not {\tluti[24]\t} } } */
/* { dg-final { scan-assembler-times {\ttbl\t} 1 } } */
