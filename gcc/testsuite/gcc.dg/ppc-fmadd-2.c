/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "f(add|sub|mul|neg)" } } */

void foo(double *a, double *b, double *c, double *d)
{
  a[0] =  b[0] + c[0] * d[0];		// fmadd
  a[2] = -b[2] + c[2] * d[2];   	// fmsub
  a[4] = -( b[4] + c[4] * d[4]);	// fnmadd
  a[6] = -(-b[6] + c[6] * d[6]);	// fnmsub
  a[10] =  b[10] - c[10] * -d[10];	// fmadd
  a[12] = -b[12] - c[12] * -d[12];   	// fmsub
  a[14] = -( b[14] - c[14] * -d[14]);	// fnmadd
  a[16] = -(-b[16] - c[16] * -d[16]);	// fnmsub
}

void foos(float *a, float *b, float *c, float *d)
{
  a[0] =  b[0] + c[0] * d[0];		// fmadd
  a[2] = -b[2] + c[2] * d[2];   	// fmsub
  a[4] = -( b[4] + c[4] * d[4]);	// fnmadd
  a[6] = -(-b[6] + c[6] * d[6]);	// fnmsub
  a[10] =  b[10] - c[10] * -d[10];	// fmadd
  a[12] = -b[12] - c[12] * -d[12];   	// fmsub
  a[14] = -( b[14] - c[14] * -d[14]);	// fnmadd
  a[16] = -(-b[16] - c[16] * -d[16]);	// fnmsub
}
