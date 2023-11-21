/* { dg-do assemble } */
/* { dg-options "-O3 --save-temps --param=vect-epilogues-nomask=0" } */

#pragma GCC target "+nosve"

#define SIGN unsigned
#define TYPE1 int
#define TYPE2 long long

void d2 (SIGN TYPE2 * restrict a, SIGN TYPE1 *b, int n)
{
    for (int i = 0; i < (n & -8); i++)
      a[i] = b[i];
}

/* { dg-final { scan-assembler-times {\tzip1\t} 1 } } */
/* { dg-final { scan-assembler-times {\tzip2\t} 1 } } */
/* { dg-final { scan-assembler-not {\tuxtl\t} } } */
/* { dg-final { scan-assembler-not {\tuxtl2\t} } } */

