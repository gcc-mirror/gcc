/*
  Program to test complex divide for correct results on selected values.
  Checking known failure points.
*/

#include <float.h>

extern void abort (void);
extern void exit (int);

extern int ilogbf (float);
int match (float _Complex, float _Complex);

#define SMALL FLT_MIN
#define MAXBIT FLT_MANT_DIG
#define ERRLIM 6

/*
  Compare c (computed value) with z (expected value).
  Return 0 if within allowed range.  Return 1 if not.
*/
int match (float _Complex c, float _Complex z)
{
  float rz, iz, rc, ic;
  float rerr, ierr, rmax;
  int biterr;
  rz = __real__ z;
  iz = __imag__ z;
  rc = __real__ c;
  ic = __imag__ c;

  if (__builtin_fabsf (rz) > SMALL)
    {
      rerr = __builtin_fabsf (rz - rc) / __builtin_fabsf (rz);
    }
  else if (__builtin_fabsf (rz) == 0.0)
    {
      rerr = __builtin_fabsf (rc);
    }
  else
    {
      rerr = __builtin_fabsf (rz - rc) / SMALL;
    }

  if (__builtin_fabsf (iz) > SMALL)
    {
      ierr = __builtin_fabsf (iz - ic) / __builtin_fabsf (iz);
    }
  else if (__builtin_fabsf (iz) == 0.0)
    {
      ierr = __builtin_fabsf (ic);
    }
  else
    {
      ierr = __builtin_fabsf (iz - ic) / SMALL;
    }
  rmax = __builtin_fmaxf(rerr, ierr);
  biterr = 0;
  if ( rmax != 0.0)      
    {
      biterr = ilogbf (rmax) + MAXBIT + 1;
    }

  if (biterr >= ERRLIM)
    return 0;
  else
    return 1;
}


int main(int argc, char** argv)
{
  float _Complex a,b,c,z;
  float xr[4], xi[4], yr[4], yi[4], zr[4], zi[4];
  float cr, ci;
  int i;
  int ok = 1;
  xr[0] = 0x1.0b1600p-133;
  xi[0] = 0x1.5e1c28p+54;
  yr[0] = -0x1.cdec8cp-119;
  yi[0] = 0x1.1e72ccp+32;
  zr[0] = 0x1.38e502p+22;
  zi[0] = -0x1.f89220p-129;

  xr[1] = -0x1.b1bee2p+121;
  xi[1] = -0x1.cb403ep-59;
  yr[1] = 0x1.480000p-144;
  yi[1] = -0x1.c66fc4p+5;
  zr[1] = -0x1.60b8cap-34;
  zi[1] = -0x1.e8b02ap+115;

  xr[2] = -0x1.3f6e00p-97;
  xi[2] = -0x1.c00000p-146;
  yr[2] = 0x1.000000p-148;
  yi[2] = -0x1.0c4e70p-91;
  zr[2] = 0x1.aa50d0p-55;
  zi[2] = -0x1.30c746p-6;

  xr[3] = 0x1.000000p-148;
  xi[3] = 0x1.f4bc04p-84;
  yr[3] = 0x1.00ad74p-20;
  yi[3] = 0x1.2ad02ep-85;
  zr[3] = 0x1.1102ccp-127;
  zi[3] = 0x1.f369a4p-64;

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
