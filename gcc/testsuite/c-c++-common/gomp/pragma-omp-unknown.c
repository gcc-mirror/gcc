/* { dg-do preprocess } */
/* { dg-options "-fopenmp" } */

#define X UNKNOWN1
#pragma omp X
/* { dg-final { scan-file pragma-omp-unknown.i "#pragma omp UNKNOWN1" } } */

#define Y UNKNOWN2
_Pragma("omp Y")
/* { dg-final { scan-file pragma-omp-unknown.i "#pragma omp UNKNOWN2" } } */
