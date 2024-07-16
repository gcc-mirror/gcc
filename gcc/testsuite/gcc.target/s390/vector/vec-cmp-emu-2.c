/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13" } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler-times {\tvchg\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvchlg\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvceqg\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvpdi\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvn\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvo\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvrepg\t} 1 } } */

typedef __attribute__ ((vector_size (16))) __int128 v1ti;

v1ti
gt (v1ti x, v1ti y)
{
  return x > y;
}
