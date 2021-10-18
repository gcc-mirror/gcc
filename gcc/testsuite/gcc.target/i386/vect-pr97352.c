/* { dg-do compile } */
/* { dg-options "-O3 -mavx -mno-avx2 -mno-avx512f" } */

double x[2], a[4], b[4], c[5];

void foo ()
{
  a[0] = c[0];
  a[1] = c[1];
  a[2] = c[0];
  a[3] = c[1];
  b[0] = c[2];
  b[1] = c[3];
  b[2] = c[2];
  b[3] = c[3];
  x[0] = c[4];
  x[1] = c[4];
}

/* We should vectorize all three stores and the load from c apart
   from c[4] which should be duped.  */
/* { dg-final { scan-assembler-times "vmov.pd" 4 } } */
