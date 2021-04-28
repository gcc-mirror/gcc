/*
  Program to test complex divide for correct results on selected values.
  Checking known failure points.
*/

#include <float.h>

extern void abort (void);
extern void exit (int);

extern int ilogbl (long double);
int match (long double _Complex,long double _Complex);

#define SMALL LDBL_MIN
#define MAXBIT LDBL_MANT_DIG
#define ERRLIM 6

/*
  Compare c (computed value) with z (expected value).
  Return 0 if within allowed range.  Return 1 if not.
*/
int match (long double _Complex c,long double _Complex z)
{
  long double rz, iz, rc, ic;
  long double rerr, ierr, rmax;
  int biterr;
  rz = __real__ z;
  iz = __imag__ z;
  rc = __real__ c;
  ic = __imag__ c;

  if (__builtin_fabsl (rz) > SMALL)
    {
      rerr = __builtin_fabsl (rz - rc) / __builtin_fabsl(rz);
    }
  else if (__builtin_fabsl (rz) == 0.0)
    {
      rerr = __builtin_fabsl (rc);
    }
  else
    {
      rerr = __builtin_fabsl (rz - rc) / SMALL;
    }

  if (__builtin_fabsl (iz) > SMALL)
    {
      ierr = __builtin_fabsl (iz - ic) / __builtin_fabsl(iz);
    }
  else if (__builtin_fabsl (iz) == 0.0)
    {
      ierr = __builtin_fabsl (ic);
    }
  else
    {
      ierr = __builtin_fabsl (iz - ic) / SMALL;
    }
  rmax = __builtin_fmaxl (rerr, ierr);
  biterr = 0;
  if ( rmax != 0.0)      
    {
      biterr = ilogbl (rmax) + MAXBIT + 1;
    }

  if (biterr >= ERRLIM)
    return 0;
  else
    return 1;
}


int main (int argc, char** argv)
{
  long double _Complex a,b,c,z;
  long double xr[4], xi[4], yr[4], yi[4], zr[4], zi[4];
  long double cr, ci;
  int i;
  int ok = 1;

#if (LDBL_MAX_EXP < 2048)
  /*
    Test values when mantissa is 11 or fewer bits.  Either LDBL is
    using DBL on this platform or we are using IBM extended double
    precision. Test values will be automatically truncated when
    the available precision is smaller than the explicit precision.
  */
  xr[0] = -0x1.16e7fad79e45ep+651;
  xi[0] = -0x1.f7f75b94c6c6ap-860;
  yr[0] = -0x1.2f40d8ff7e55ep+245;
  yi[0] = -0x0.0000000004ebcp-968;
  zr[0] = 0x1.d6e4b0e2828694570ba839070beep+405L;
  zi[0] = -0x1.e9095e311e70498db810196259b7p-846L;

  xr[1] = -0x1.21ff587f953d3p-310;
  xi[1] = -0x1.5a526dcc59960p+837;
  yr[1] = 0x1.b88b8b552eaadp+735;
  yi[1] = -0x1.873e2d6544d92p-327;
  zr[1] = 0x1.65734a88b2ddff699c482ee8eef6p-961L;
  zi[1] = -0x1.927e85b8b576f94a797a1bcb733dp+101L;

  xr[2] = 0x1.4612e41aa8080p-846;
  xi[2] = -0x0.0000000613e07p-968;
  yr[2] = 0x1.df9cd0d58caafp-820;
  yi[2] = -0x1.e47051a9036dbp-584;
  zr[2] = 0x1.9b194f3aaadea545174c5372d8p-415L;
  zi[2] = 0x1.58a00ab740a6ad3249002f2b79p-263L;

  xr[3] = 0x1.cb27eece7c585p-355;
  xi[3] = 0x0.000000223b8a8p-968;
  yr[3] = -0x1.74e7ed2b9189fp-22;
  yi[3] = 0x1.3d80439e9a119p-731;
  zr[3] = -0x1.3b35ed806ae5a2a8cc1c9a96931dp-333L;
  zi[3] = -0x1.7802c17c774895bd541adeb200p-974L;
#else
  /*
    Test values intended for either IEEE128 or Intel80 formats.  In
    either case, 15 bits of exponent are available.  Test values will
    be automatically truncated when the available precision is smaller
    than the explicit precision.
  */
  xr[0] = -0x9.c793985b7d029d90p-8480L;
  xi[0] = 0x8.018745ffa61a8fe0p+16329L;
  yr[0] = -0xe.d5bee9c523a35ad0p-15599L;
  yi[0] = -0xa.8c93c5a4f94128f0p+869L;
  zr[0] = -0x1.849178451c035b95d16311d0efdap+15459L;
  zi[0] = -0x1.11375ed2c1f58b9d047ab64aed97p-1008L;

  xr[1] = 0xb.68e44bc6d0b91a30p+16026L;
  xi[1] = 0xb.ab10f5453e972f30p-14239L;
  yr[1] = 0x8.8cbd470705428ff0p-16350L;
  yi[1] = -0xa.0c1cbeae4e4b69f0p+347L;
  zr[1] = 0x1.eec40848785e500d9f0945ab58d3p-1019L;
  zi[1] = 0x1.22b6b579927a3f238b772bb6dc95p+15679L;

  xr[2] = -0x9.e8c093a43b546a90p+15983L;
  xi[2] = 0xc.95b18274208311e0p-2840L;
  yr[2] = -0x8.dedb729b5c1b2ec0p+8L;
  yi[2] = 0xa.a49fb81b24738370p-16385L;
  zr[2] = 0x1.1df99ee89bb118f3201369e06576p+15975L;
  zi[2] = 0x1.571e7ef904d6b6eee7acb0dcf098p-418L;

  xr[3] = 0xc.4687f251c0f48bd0p-3940L;
  xi[3] = -0xe.a3f2138992d85fa0p+15598L;
  yr[3] = 0xe.4b0c25c3d5ebb830p-16344L;
  yi[3] = -0xa.6cbf1ba80f7b97a0p+78L;
  zr[3] = 0x1.6785ba23bfb744cee97b4142348bp+15520L;
  zi[3] = -0x1.ecee7b8c7bdd36237eb538324289p-902L;
#endif

  for (i = 0; i < 4; i++)
    {
      __real__ a = xr[i];
      __imag__ a = xi[i];
      __real__ b = yr[i];
      __imag__ b = yi[i];
      __real__ z = zr[i];
      __imag__ z = zi[i];
      c = a / b;
      cr = __real__ c;
      ci = __imag__ c;

      if (!match (c,z)){
	ok = 0;
      }
    }
  if (!ok)
    abort ();
  exit (0);
}
