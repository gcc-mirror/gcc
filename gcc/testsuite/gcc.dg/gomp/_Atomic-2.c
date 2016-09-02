/* PR c/65467 */
/* { dg-do compile } */
/* { dg-additional-options "-std=c11" } */

void
f1 (void)
{
  _Atomic int i;
  #pragma omp for		/* { dg-error "'_Atomic' iteration variable 'i'" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp parallel for	/* { dg-error "'_Atomic' iteration variable 'i'" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp simd		/* { dg-error "'_Atomic' iteration variable 'i'" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp parallel for simd	/* { dg-error "'_Atomic' iteration variable 'i'" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for simd		/* { dg-error "'_Atomic' iteration variable 'i'" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp for		/* { dg-error "'_Atomic' iteration variable 'j'" } */
  for (_Atomic int j = 0; j < 64; j++)
    ;
  #pragma omp parallel for	/* { dg-error "'_Atomic' iteration variable 'j'" } */
  for (_Atomic int j = 0; j < 64; j++)
    ;
  #pragma omp simd		/* { dg-error "'_Atomic' iteration variable 'j'" } */
  for (_Atomic int j = 0; j < 64; j++)
    ;
  #pragma omp parallel for simd	/* { dg-error "'_Atomic' iteration variable 'j'" } */
  for (_Atomic int j = 0; j < 64; j++)
    ;
  #pragma omp for simd		/* { dg-error "'_Atomic' iteration variable 'j'" } */
  for (_Atomic int j = 0; j < 64; j++)
    ;
}

void
f2 (void)
{
  _Atomic int i;
  #pragma omp distribute		/* { dg-error "'_Atomic' iteration variable 'i'" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp distribute parallel for	/* { dg-error "'_Atomic' iteration variable 'i'" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp distribute parallel for simd /* { dg-error "'_Atomic' iteration variable 'i'" } */
  for (i = 0; i < 64; i++)
    ;
  #pragma omp distribute		/* { dg-error "'_Atomic' iteration variable 'j'" } */
  for (_Atomic int j = 0; j < 64; j++)
    ;
  #pragma omp distribute parallel for	/* { dg-error "'_Atomic' iteration variable 'j'" } */
  for (_Atomic int j = 0; j < 64; j++)
    ;
  #pragma omp distribute parallel for simd /* { dg-error "'_Atomic' iteration variable 'j'" } */
  for (_Atomic int j = 0; j < 64; j++)
    ;
}

void
f3 (void)
{
  int i;
  _Atomic int j = 0;
  #pragma omp simd linear(j:2)		/* { dg-error "'_Atomic' 'j' in 'linear' clause" } */
  for (i = 0; i < 64; i++)
    j += 2;
  #pragma omp parallel for linear(j:1)	/* { dg-error "'_Atomic' 'j' in 'linear' clause" } */
  for (i = 0; i < 64; i++)
    j++;
}
