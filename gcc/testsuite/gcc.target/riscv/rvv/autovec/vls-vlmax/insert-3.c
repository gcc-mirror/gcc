/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

#include <stdint-gcc.h>

typedef double vnx2df __attribute__ ((vector_size (16)));
typedef double vnx4df __attribute__ ((vector_size (32)));
typedef double vnx8df __attribute__ ((vector_size (64)));
typedef double vnx16df __attribute__ ((vector_size (128)));

__attribute__ ((noipa)) void
f_vnx2df (double a, double b, double *out)
{
  vnx2df v = {a, b};
  *(vnx2df *) out = v;
}

__attribute__ ((noipa)) void
f_vnx4df (double a, double b, double c, double d, double *out)
{
  vnx4df v = {a, b, c, d};
  *(vnx4df *) out = v;
}

__attribute__ ((noipa)) void
f_vnx8df (double a, double b, double c, double d, double e, double f, double g, double h, double *out)
{
  vnx8df v = {a, b, c, d, e, f, g, h};
  *(vnx8df *) out = v;
}

__attribute__ ((noipa)) void
f_vnx16df (double a, double b, double c, double d, double e, double f,
	   double g, double h, double a2, double b2, double c2, double d2,
	   double e2, double f2, double g2, double h2, double *out)
{
  vnx16df v = {a, b, c, d, e, f, g, h, a2, b2, c2, d2, e2, f2, g2, h2};
  *(vnx16df *) out = v;
}

/* { dg-final { scan-assembler-times {vfslide1down\.vf\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 26 } } */
