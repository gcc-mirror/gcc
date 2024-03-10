/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target { x86_64-*-* i?86-*-* } } } */

float x[4];

float test1 (float a)
{
  return x[0] + x[2] + x[1] + x[3] + a;
}

float test2 (void)
{
  return x[3] + x[2] + x[1] + 1.f + x[0];
}

float test3 (float a)
{
  return x[0] + a + x[2] + x[1] + x[3] + 1.f;
}

/* We currently require a .REDUC_PLUS direct internal function but do not
   have a dejagnu target for this.  */
/* { dg-final { scan-tree-dump-times "Basic block will be vectorized using SLP" 3 "slp2" { target { x86_64-*-* i?86-*-* } } } } */
