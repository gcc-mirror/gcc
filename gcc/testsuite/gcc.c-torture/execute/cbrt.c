/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
*/

void abort (void);
void exit (int);

#ifndef __vax__
static const unsigned long
	B1 = 715094163, /* B1 = (682-0.03306235651)*2**20 */
	B2 = 696219795; /* B2 = (664-0.03306235651)*2**20 */

static const double
	C =  5.42857142857142815906e-01, /* 19/35     = 0x3FE15F15, 0xF15F15F1 */
	D = -7.05306122448979611050e-01, /* -864/1225 = 0xBFE691DE, 0x2532C834 */
	E =  1.41428571428571436819e+00, /* 99/70     = 0x3FF6A0EA, 0x0EA0EA0F */
	F =  1.60714285714285720630e+00, /* 45/28     = 0x3FF9B6DB, 0x6DB6DB6E */
	G =  3.57142857142857150787e-01; /* 5/14      = 0x3FD6DB6D, 0xB6DB6DB7 */

double
cbrtl (double x)
{
  long hx;
  double r,s,w;
  double lt;
  unsigned sign;
  typedef unsigned unsigned32 __attribute__((mode(SI)));
  union {
    double t;
    unsigned32 pt[2];
  } ut, ux;
  int n0;

  ut.t = 1.0;
  n0 = (ut.pt[0] == 0);

  ut.t = 0.0;
  ux.t = x;

  hx = ux.pt[n0];			/* high word of x */
  sign=hx&0x80000000;			/* sign= sign(x) */
  hx  ^=sign;
  if(hx>=0x7ff00000) return(x+x);	/* cbrt(NaN,INF) is itself */
  if((hx| ux.pt[1-n0])==0)
    return(ux.t);			/* cbrt(0) is itself */

  ux.pt[n0] = hx;
  /* rough cbrt to 5 bits */
  if(hx<0x00100000)			/* subnormal number */
    {ut.pt[n0]=0x43500000;		/* set t= 2**54 */
     ut.t*=x; ut.pt[n0]=ut.pt[n0]/3+B2;
   }
  else
    ut.pt[n0]=hx/3+B1;

  /* new cbrt to 23 bits, may be implemented in single precision */
  r=ut.t*ut.t/ux.t;
  s=C+r*ut.t;
  ut.t*=G+F/(s+E+D/s);

  /* chopped to 20 bits and make it larger than cbrt(x) */
  ut.pt[1-n0]=0; ut.pt[n0]+=0x00000001;

  /* one step newton iteration to 53 bits with error less than 0.667 ulps */
  s=ut.t*ut.t;				/* t*t is exact */
  r=ux.t/s;
  w=ut.t+ut.t;
  r=(r-ut.t)/(w+r);			/* r-s is exact */
  ut.t=ut.t+ut.t*r;

  /* restore the sign bit */
  ut.pt[n0] |= sign;

  lt = ut.t;
  lt -= (lt - (x/(lt*lt))) * 0.333333333333333333333;
  return lt;
}

int
main (void)
{
  if ((int) (cbrtl (27.0) + 0.5) != 3)
    abort ();

  exit (0);
}
#else
int main (void) { exit (0); }
#endif
