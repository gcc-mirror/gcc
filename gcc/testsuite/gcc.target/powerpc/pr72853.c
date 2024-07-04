/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O3 -funroll-loops" } */
/* { dg-require-effective-target powerpc_vsx } */

/* derived from 20021120-1.c, compiled for -mcpu=power9.  */

extern void abort (void);
extern void exit (int);

volatile double gd[32];
volatile float gf[32];

void
foo (int n)
{
  double d00, d10, d20, d30, d01, d11, d21, d31, d02, d12, d22, d32, d03, d13,
    d23, d33, d04, d14, d24, d34, d05, d15, d25, d35, d06, d16, d26, d36, d07,
    d17, d27, d37;
  float f00, f10, f20, f30, f01, f11, f21, f31, f02, f12, f22, f32, f03, f13,
    f23, f33, f04, f14, f24, f34, f05, f15, f25, f35, f06, f16, f26, f36, f07,
    f17, f27, f37;
  volatile double *pd;
  volatile float *pf;
  int i;

  pd = gd;
  d00 = *(pd++), d10 = *(pd++), d20 = *(pd++), d30 = *(pd++), d01 =
    *(pd++), d11 = *(pd++), d21 = *(pd++), d31 = *(pd++), d02 = *(pd++), d12 =
    *(pd++), d22 = *(pd++), d32 = *(pd++), d03 = *(pd++), d13 = *(pd++), d23 =
    *(pd++), d33 = *(pd++), d04 = *(pd++), d14 = *(pd++), d24 = *(pd++), d34 =
    *(pd++), d05 = *(pd++), d15 = *(pd++), d25 = *(pd++), d35 = *(pd++), d06 =
    *(pd++), d16 = *(pd++), d26 = *(pd++), d36 = *(pd++), d07 = *(pd++), d17 =
    *(pd++), d27 = *(pd++), d37 = *(pd++);
  for (i = 0; i < n; i++)
    {
      pf = gf;
      f00 = *(pf++), f10 = *(pf++), f20 = *(pf++), f30 = *(pf++), f01 =
	*(pf++), f11 = *(pf++), f21 = *(pf++), f31 = *(pf++), f02 =
	*(pf++), f12 = *(pf++), f22 = *(pf++), f32 = *(pf++), f03 =
	*(pf++), f13 = *(pf++), f23 = *(pf++), f33 = *(pf++), f04 =
	*(pf++), f14 = *(pf++), f24 = *(pf++), f34 = *(pf++), f05 =
	*(pf++), f15 = *(pf++), f25 = *(pf++), f35 = *(pf++), f06 =
	*(pf++), f16 = *(pf++), f26 = *(pf++), f36 = *(pf++), f07 =
	*(pf++), f17 = *(pf++), f27 = *(pf++), f37 = *(pf++);
      pd = gd;
      d00 += *(pd++), d10 += *(pd++), d20 += *(pd++), d30 += *(pd++), d01 +=
	*(pd++), d11 += *(pd++), d21 += *(pd++), d31 += *(pd++), d02 +=
	*(pd++), d12 += *(pd++), d22 += *(pd++), d32 += *(pd++), d03 +=
	*(pd++), d13 += *(pd++), d23 += *(pd++), d33 += *(pd++), d04 +=
	*(pd++), d14 += *(pd++), d24 += *(pd++), d34 += *(pd++), d05 +=
	*(pd++), d15 += *(pd++), d25 += *(pd++), d35 += *(pd++), d06 +=
	*(pd++), d16 += *(pd++), d26 += *(pd++), d36 += *(pd++), d07 +=
	*(pd++), d17 += *(pd++), d27 += *(pd++), d37 += *(pd++);
      pd = gd;
      d00 += *(pd++), d10 += *(pd++), d20 += *(pd++), d30 += *(pd++), d01 +=
	*(pd++), d11 += *(pd++), d21 += *(pd++), d31 += *(pd++), d02 +=
	*(pd++), d12 += *(pd++), d22 += *(pd++), d32 += *(pd++), d03 +=
	*(pd++), d13 += *(pd++), d23 += *(pd++), d33 += *(pd++), d04 +=
	*(pd++), d14 += *(pd++), d24 += *(pd++), d34 += *(pd++), d05 +=
	*(pd++), d15 += *(pd++), d25 += *(pd++), d35 += *(pd++), d06 +=
	*(pd++), d16 += *(pd++), d26 += *(pd++), d36 += *(pd++), d07 +=
	*(pd++), d17 += *(pd++), d27 += *(pd++), d37 += *(pd++);
      pd = gd;
      d00 += *(pd++), d10 += *(pd++), d20 += *(pd++), d30 += *(pd++), d01 +=
	*(pd++), d11 += *(pd++), d21 += *(pd++), d31 += *(pd++), d02 +=
	*(pd++), d12 += *(pd++), d22 += *(pd++), d32 += *(pd++), d03 +=
	*(pd++), d13 += *(pd++), d23 += *(pd++), d33 += *(pd++), d04 +=
	*(pd++), d14 += *(pd++), d24 += *(pd++), d34 += *(pd++), d05 +=
	*(pd++), d15 += *(pd++), d25 += *(pd++), d35 += *(pd++), d06 +=
	*(pd++), d16 += *(pd++), d26 += *(pd++), d36 += *(pd++), d07 +=
	*(pd++), d17 += *(pd++), d27 += *(pd++), d37 += *(pd++);
      pf = gf;
      *(pf++) = f00, *(pf++) = f10, *(pf++) = f20, *(pf++) = f30, *(pf++) =
	f01, *(pf++) = f11, *(pf++) = f21, *(pf++) = f31, *(pf++) =
	f02, *(pf++) = f12, *(pf++) = f22, *(pf++) = f32, *(pf++) =
	f03, *(pf++) = f13, *(pf++) = f23, *(pf++) = f33, *(pf++) =
	f04, *(pf++) = f14, *(pf++) = f24, *(pf++) = f34, *(pf++) =
	f05, *(pf++) = f15, *(pf++) = f25, *(pf++) = f35, *(pf++) =
	f06, *(pf++) = f16, *(pf++) = f26, *(pf++) = f36, *(pf++) =
	f07, *(pf++) = f17, *(pf++) = f27, *(pf++) = f37;
    }
  pd = gd;
  *(pd++) = d00, *(pd++) = d10, *(pd++) = d20, *(pd++) = d30, *(pd++) =
    d01, *(pd++) = d11, *(pd++) = d21, *(pd++) = d31, *(pd++) = d02, *(pd++) =
    d12, *(pd++) = d22, *(pd++) = d32, *(pd++) = d03, *(pd++) = d13, *(pd++) =
    d23, *(pd++) = d33, *(pd++) = d04, *(pd++) = d14, *(pd++) = d24, *(pd++) =
    d34, *(pd++) = d05, *(pd++) = d15, *(pd++) = d25, *(pd++) = d35, *(pd++) =
    d06, *(pd++) = d16, *(pd++) = d26, *(pd++) = d36, *(pd++) = d07, *(pd++) =
    d17, *(pd++) = d27, *(pd++) = d37;
}

int
main ()
{
  int i;

  for (i = 0; i < 32; i++)
    gd[i] = i, gf[i] = i;
  foo (1);
  for (i = 0; i < 32; i++)
    if (gd[i] != i * 4 || gf[i] != i)
      abort ();
  exit (0);
}

/* { dg-final { scan-assembler-not "stxsd \[0-9\]+,\[0-9\]+,\[0-9\]"  } } */
