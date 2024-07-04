/* { dg-additional-options "-fpermissive" } */

typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef union
{
  double value;
  struct
  {
    __uint32_t msw;
    __uint32_t lsw;
  } parts;
} ieee_double_shape_type;
two52= 4.50359962737049600000e+15,
w6 = -1.63092934096575273989e-03;
double sin_pi(double x)
{
 double y,z;
 __int32_t n,ix;
ieee_double_shape_type gh_u;
 gh_u.value = (x); 
(ix) = gh_u.parts.msw;
                if(ix<0x43300000)
 z = y+two52;
 ieee_double_shape_type gl_u;
 gl_u.value = (z);
 (n) = gl_u.parts.lsw;
  n &= 1;
 switch (n)
 {
     case 0:
__kernel_sin();
     }
}
