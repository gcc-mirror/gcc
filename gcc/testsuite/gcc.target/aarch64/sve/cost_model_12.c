/* { dg-options "-O3 -mtune=neoverse-512tvb" } */

void
f (float x[restrict 10][1024],
   float y[restrict 10][1024], float z)
{
  for (int i = 0; i < 10; ++i)
    {
#pragma GCC unroll 10
      for (int j = 0; j < 10; ++j)
	x[j][i] = y[j][i] * z;
    }
}

/* We should unroll the outer loop, with 2x 16-byte vectors and 1x
   8-byte vectors.  */
/* { dg-final { scan-assembler-not {\tptrue\t} } } */
/* { dg-final { scan-assembler {\tv[0-9]+\.4s,} } } */
/* { dg-final { scan-assembler {\tv[0-9]+\.2s,} } } */
