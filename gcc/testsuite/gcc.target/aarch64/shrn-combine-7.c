/* { dg-do assemble } */
/* { dg-options "-O3 --save-temps --param=vect-epilogues-nomask=0" } */

#pragma GCC target "+nosve"

#define TYPE1 int
#define TYPE2 long long
#define SHIFT 32

void foo (TYPE2 * restrict a, TYPE1 * restrict d, int n)
{
    for( int i = 0; i < n; i++ )
      d[i] = a[i] >> SHIFT;
}

/* { dg-final { scan-assembler-times {\tuzp2\t} 1 } } */
/* { dg-final { scan-assembler-not {\tshrn\t} } } */
/* { dg-final { scan-assembler-not {\tshrn2\t} } } */
