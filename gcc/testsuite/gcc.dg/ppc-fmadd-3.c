/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "f(add|sub|mul)" } } */

void foo(double *a, double *b, double *c, double *d)
{
#if 0
  a[1] =  b[1] - c[1] * d[1];		// fneg, fmadd without fast-math
#endif
  a[3] = -b[3] - c[3] * d[3];		// fneg, fmsub without fast-math
#if 0
  a[5] = -( b[5] - c[5] * d[5]);	// fneg, fnmadd without fast-math
#endif
  a[7] = -(-b[7] - c[7] * d[7]);	// fneg, fnmsub without fast-math
  a[11] =  b[11] + c[11] * -d[11];	// fneg, fmadd without fast-math
  a[13] = -b[13] + c[13] * -d[13];	// fneg, fmsub without fast-math
  a[15] = -( b[15] + c[15] * -d[15]);	// fneg, fnmadd without fast-math
  a[17] = -(-b[17] + c[17] * -d[17]);	// fneg, fnmsub without fast-math
}

void foos(float *a, float *b, float *c, float *d)
{
#if 0
  a[1] =  b[1] - c[1] * d[1];		// fneg, fmadd without fast-math
#endif
  a[3] = -b[3] - c[3] * d[3];		// fneg, fmsub without fast-math
#if 0
  a[5] = -( b[5] - c[5] * d[5]);	// fneg, fnmadd without fast-math
#endif
  a[7] = -(-b[7] - c[7] * d[7]);	// fneg, fnmsub without fast-math
  a[11] =  b[11] + c[11] * -d[11];	// fneg, fmadd without fast-math
  a[13] = -b[13] + c[13] * -d[13];	// fneg, fmsub without fast-math
  a[15] = -( b[15] + c[15] * -d[15]);	// fneg, fnmadd without fast-math
  a[17] = -(-b[17] + c[17] * -d[17]);	// fneg, fnmsub without fast-math
}

