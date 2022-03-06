/* { dg-do assemble } */
/* { dg-options "-O3 --save-temps --param=vect-epilogues-nomask=0" } */

#pragma GCC target "+nosve"

#define TYPE short

void e (signed TYPE * restrict a, signed TYPE *b, int n)
{
    for (int i = 0; i < n; i++)
      b[i] = a[i] >> (sizeof(TYPE)*8)-1;
}

/* { dg-final { scan-assembler-times {\tcmlt\t} 1 } } */
/* { dg-final { scan-assembler-not {\tsshr\t} } } */

