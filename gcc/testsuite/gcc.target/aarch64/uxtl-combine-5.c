/* { dg-do assemble } */
/* { dg-options "-O3 --save-temps --param=vect-epilogues-nomask=0" } */

#pragma GCC target "+nosve"

#define SIGN signed
#define TYPE1 short
#define TYPE2 int

void d2 (SIGN TYPE2 * restrict a, SIGN TYPE1 *b, int n)
{
    for (int i = 0; i < (n & -8); i++)
      a[i] = b[i];
}

/* { dg-final { scan-assembler-not {\tzip1\t} } } */
/* { dg-final { scan-assembler-not {\tzip2\t} } } */
/* { dg-final { scan-assembler-times {\tsxtl\t} 1 } } */
/* { dg-final { scan-assembler-times {\tsxtl2\t} 1 } } */

