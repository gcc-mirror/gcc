/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-ffast-math -O2" } */
/* { dg-final { scan-assembler-not "f(add|sub|mul|neg)" } } */

void foo(double *a, double *b, double *c, double *d)
{
  a[0] =  b[0] + c[0] * d[0];		// fmadd
  a[1] =  b[1] - c[1] * d[1];		// fnmsub with fast-math
  a[2] = -b[2] + c[2] * d[2];   	// fmsub
  a[3] = -b[3] - c[3] * d[3];		// fnmadd with fast-math
  a[4] = -( b[4] + c[4] * d[4]);	// fnmadd
  a[5] = -( b[5] - c[5] * d[5]);	// fmsub with fast-math
  a[6] = -(-b[6] + c[6] * d[6]);	// fnmsub
  a[7] = -(-b[7] - c[7] * d[7]);	// fmadd with fast-math
  a[10] =  b[10] - c[10] * -d[10];	// fmadd
  a[11] =  b[11] + c[11] * -d[11];	// fnmsub with fast-math
  a[12] = -b[12] - c[12] * -d[12];   	// fmsub
  a[13] = -b[13] + c[13] * -d[13];	// fnmadd with fast-math
  a[14] = -( b[14] - c[14] * -d[14]);	// fnmadd
  a[15] = -( b[15] + c[15] * -d[15]);	// fmsub with fast-math
  a[16] = -(-b[16] - c[16] * -d[16]);	// fnmsub
  a[17] = -(-b[17] + c[17] * -d[17]);	// fmadd with fast-math
}

void foos(float *a, float *b, float *c, float *d)
{
  a[0] =  b[0] + c[0] * d[0];		// fmadd
  a[1] =  b[1] - c[1] * d[1];		// fnmsub with fast-math
  a[2] = -b[2] + c[2] * d[2];   	// fmsub
  a[3] = -b[3] - c[3] * d[3];		// fnmadd with fast-math
  a[4] = -( b[4] + c[4] * d[4]);	// fnmadd
  a[5] = -( b[5] - c[5] * d[5]);	// fmsub with fast-math
  a[6] = -(-b[6] + c[6] * d[6]);	// fnmsub
  a[7] = -(-b[7] - c[7] * d[7]);	// fmadd with fast-math
  a[10] =  b[10] - c[10] * -d[10];	// fmadd
  a[11] =  b[11] + c[11] * -d[11];	// fnmsub with fast-math
  a[12] = -b[12] - c[12] * -d[12];   	// fmsub
  a[13] = -b[13] + c[13] * -d[13];	// fnmadd with fast-math
  a[14] = -( b[14] - c[14] * -d[14]);	// fnmadd
  a[15] = -( b[15] + c[15] * -d[15]);	// fmsub with fast-math
  a[16] = -(-b[16] - c[16] * -d[16]);	// fnmsub
  a[17] = -(-b[17] + c[17] * -d[17]);	// fmadd with fast-math
}
