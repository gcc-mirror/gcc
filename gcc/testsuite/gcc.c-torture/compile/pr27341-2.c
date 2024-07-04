/* { dg-additional-options "-fpermissive" } */
void zgemm_ (const int*, const double*);
extern void matmul_c8 (_Complex double * dest)
{
  const int  ldc = 0;
  const double zero = 0;
  zgemm_ ( &zero, &ldc);
  dest[1] += 1 ;
}
