/* { dg-do compile } */
/* { dg-options "-fopenmp-simd" } */
#ifdef __cplusplus
extern "C" {
#endif
#pragma omp declare simd
int __attribute__ ((const)) f00 (int a , char b) /* { dg-warning {GCC does not currently support a simdclone with simdlens 8 and 16 for these types.} } */
{
  return a + b;
}

#pragma omp declare simd
long long __attribute__ ((const)) f01 (int a , short b) /* { dg-warning {GCC does not currently support a simdclone with simdlens 4 and 8 for these types.} } */
{
  return a + b;
}

#pragma omp declare simd linear(b)
long long __attribute__ ((const)) f02 (short *b, int a) /* { dg-warning {GCC does not currently support a simdclone with simdlens 4 and 8 for these types.} } */
{
  return a + *b;
}

#pragma omp declare simd uniform(b)
void f03 (char b, int a) /* { dg-warning {GCC does not currently support a simdclone with simdlens 8 and 16 for these types.} } */
{
}

#pragma omp declare simd simdlen(4)
double f04 (void) /* { dg-warning {GCC does not currently support simdlen 4 for type 'double'} } */
{
  return 4;
}

#pragma omp declare simd simdlen(16)
void f05 (short a) /* { dg-warning {GCC does not currently support simdlen 16 for type 'short int'} } */
{
}
#ifdef __cplusplus
}
#endif

