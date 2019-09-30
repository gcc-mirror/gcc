/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

typedef double Double16 __attribute__((vector_size(8*16)));

void mult(Double16 *res, const Double16 *v1, const Double16 *v2)
{
  *res = *v1 * *v2;
}

/* We want 4 ymm loads and 4 ymm stores.  */
/* { dg-final { scan-assembler-times "movapd" 8 } } */
