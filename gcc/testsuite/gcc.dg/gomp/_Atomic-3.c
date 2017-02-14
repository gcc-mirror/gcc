/* PR c/65467 */
/* { dg-do compile } */
/* { dg-additional-options "-std=c11" } */

void
f1 (void)
{
  _Atomic int i = 0, k[4];
  int j = 0;
  k[0] = 0;
  k[1] = 0;
  k[2] = 0;
  k[3] = 0;
  #pragma omp parallel reduction (+:i)		/* { dg-error "'_Atomic' 'i' in 'reduction' clause" } */
    i++;
  #pragma omp declare reduction (foo: _Atomic int: omp_out += omp_in) initializer (omp_priv = omp_orig * 0)	/* { dg-error "'_Atomic' qualified type in '#pragma omp declare reduction'" } */
  #pragma omp declare reduction (bar: int: omp_out += omp_in) initializer (omp_priv = omp_orig * 0)
  #pragma omp parallel reduction (bar:j)
    j++;
  #pragma omp parallel reduction (bar:i)	/* { dg-error "'_Atomic' 'i' in 'reduction' clause" } */
    i++;
  #pragma omp parallel reduction (+:k)		/* { dg-error "'_Atomic' 'k' in 'reduction' clause" } */
    k[1]++;
  #pragma omp parallel reduction (+:k[1:2])	/* { dg-error "'_Atomic' \[^\n\r]* in 'reduction' clause" } */
    k[1]++;
}

void
f2 (int *_Atomic p)
{
  #pragma omp simd aligned (p : 16)		/* { dg-error "'_Atomic' 'p' in 'aligned' clause" } */
  for (int i = 0; i < 16; i++)
    p[i]++;
}

_Atomic int x;

void
f3 (_Atomic int *p)
{
  int i;
  #pragma omp atomic write
  x = 6;					/* { dg-error "'_Atomic' expression in '#pragma omp atomic'" } */
  #pragma omp atomic read
  i = x;					/* { dg-error "'_Atomic' expression in '#pragma omp atomic'" } */
  #pragma omp atomic update
  x += 6;					/* { dg-error "'_Atomic' expression in '#pragma omp atomic'" } */
  #pragma omp atomic capture
  i = x *= 2;					/* { dg-error "'_Atomic' expression in '#pragma omp atomic'" } */
  #pragma omp atomic write
  p[2] = 6;					/* { dg-error "'_Atomic' expression in '#pragma omp atomic'" } */
  #pragma omp atomic read
  i = p[2];					/* { dg-error "'_Atomic' expression in '#pragma omp atomic'" } */
  #pragma omp atomic update
  p[2] += 6;					/* { dg-error "'_Atomic' expression in '#pragma omp atomic'" } */
  #pragma omp atomic capture
  i = p[2] *= 2;				/* { dg-error "'_Atomic' expression in '#pragma omp atomic'" } */
}

#pragma omp declare simd linear(x:1)		/* { dg-error "'_Atomic' 'x' in 'linear' clause" } */
int
f4 (_Atomic int x, int y)
{
  return x + y;
}
