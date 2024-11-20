/* { dg-options "-Wno-old-style-definition -O2 -mcmodel=small -fPIC" }  */
/* { dg-do compile } */
/* { dg-require-effective-target fpic } */

extern int __finite (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int __finitef (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int __signbit (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int __signbitf (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
int
__ecvt_r (value, ndigit, decpt, sign, buf, len)
     double value;
     int ndigit, *decpt, *sign, len;
     char *buf;
{
  if ((sizeof (value) == sizeof (float) ? __finitef (value) : __finite (value)) && value != 0.0)
    {
      double d;
      double f = 1.0;
      d = -value;
      if (d < 1.0e-307)
	{
	  do
	    {
	      f *= 10.0;
	    }
	  while (d * f < 1.0);
	}
    }
  if (ndigit <= 0 && len > 0)
    {
      buf[0] = '\0';
      *sign = (sizeof (value) == sizeof (float) ? __finitef (value) : __finite (value)) ? (sizeof (value) == sizeof (float) ? __signbitf (value) : __signbit (value)) != 0 : 0;
    }
}
