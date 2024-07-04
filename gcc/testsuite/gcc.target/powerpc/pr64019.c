/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -ffast-math -mdejagnu-cpu=power7" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <math.h>

typedef struct
{
  double x, y, z;
  double q, a, b, mass;
  double vx, vy, vz, vw, dx, dy, dz;
}
ATOM;
int
u_f_nonbon (lambda)
     double lambda;
{
  double r, r0, xt, yt, zt;
  double lcutoff, cutoff, get_f_variable ();
  double rdebye;
  int inbond, inangle, i;
  ATOM *a1, *a2, *bonded[10], *angled[10];
  ATOM *(*use)[];
  int uselist (), nuse, used;
  ATOM *cp, *bp;
  int a_number (), inbuffer;
  double (*buffer)[], xx, yy, zz, k;
  int invector, atomsused, ii, jj, imax;
  double (*vector)[];
  ATOM *(*atms)[];
  double dielectric;
  rdebye = cutoff / 2.;
  dielectric = get_f_variable ("dielec");
  imax = a_number ();
  for (jj = 1; jj < imax; jj++, a1 = bp)
    {
      if ((*use)[used] == a1)
	{
	  used += 1;
	}
      while ((*use)[used] != a1)
	{
	  for (i = 0; i < inbuffer; i++)
	    {
	    }
	  xx = a1->x + lambda * a1->dx;
	  yy = a1->y + lambda * a1->dy;
	  zz = a1->z + lambda * a1->dz;
	  for (i = 0; i < inbuffer; i++)
	    {
	      xt = xx - (*buffer)[3 * i];
	      yt = yy - (*buffer)[3 * i + 1];
	      zt = zz - (*buffer)[3 * i + 2];
	      r = xt * xt + yt * yt + zt * zt;
	      r0 = sqrt (r);
	      xt = xt / r0;
	      zt = zt / r0;
	      k =
		-a1->q * (*atms)[i]->q * dielectric * exp (-r0 / rdebye) *
		(1. / (rdebye * r0) + 1. / r);
	      k += a1->a * (*atms)[i]->a / r / r0 * 6;
	      k -= a1->b * (*atms)[i]->b / r / r / r0 * 12;
	      (*vector)[3 * i] = xt * k;
	      (*vector)[3 * i + 1] = yt * k;
	      (*vector)[3 * i + 2] = zt * k;
	    }
	}
    }
}
