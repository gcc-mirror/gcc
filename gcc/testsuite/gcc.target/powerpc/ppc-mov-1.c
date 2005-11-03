/* { dg-do compile { target powerpc64-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-not "fmr \[0-9\]+,\[0-9\]+" } }

/* Origin:Pete Steinmetz <steinmtz@us.ibm.com> */

/* PR 16796: Extraneous move.  */

static const double huge = 1.0e300;
typedef int int64_t __attribute__ ((__mode__ (__DI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__ (__DI__)));

double __floor(double x)
{
  union {
    double dbl_val;
    long int long_val;
  } temp;

  int64_t i0,j0;
  u_int64_t i;
  temp.dbl_val = x;
  i0 = temp.long_val;

  j0 = ((i0>>52)&0x7ff)-0x3ff;
  if(j0<52) {
    if(j0<0) {
      if(huge+x>0.0) {
        if(i0>=0) {i0=0;}
        else if((i0&0x7fffffffffffffff)!=0)
        { i0=0xbff0000000000000;}
      }
    } else {
      i = (0x000fffffffffffff)>>j0;
      if((i0&i)==0) return x;
      if(huge+x>0.0) {
        if(i0<0) i0 += (0x0010000000000000)>>j0;
        i0 &= (~i);
      }
    }
  } else {
    if (j0==0x400)
      return x+x;
    else
      return x;
  }
  temp.long_val = i0;
  x = temp.dbl_val;
  return x;
}

