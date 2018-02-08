/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv4t" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" } { "" } } */
/* { dg-require-effective-target arm_arch_v4t_ok } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-options "-mthumb" } */
/* { dg-add-options arm_arch_v4t } */
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
typedef union
{
  double value;
  struct
  {
    unsigned int msw;
  } parts;
} ieee_double_shape_type;
double __ieee754_hypot(double x, double y)
{
 double a=x,b=y,t1,t2,y1,y2,w;
 int j,k,ha,hb;
 do { ieee_double_shape_type gh_u; gh_u.value = (x); (ha) = gh_u.parts.msw; } while (0);;
 if(hb > ha) {a=y;b=x;j=ha; ha=hb;hb=j;} else {a=x;b=y;}
 if(ha > 0x5f300000) {
    do { ieee_double_shape_type sh_u; sh_u.value = (a); sh_u.parts.msw = (ha); (a) = sh_u.value; } while (0);;
 }
 w = a-b;
 if (w <= b)
 {
     t2 = a - t1;
     w = t1*y1-(w*(-w)-(t1*y2+t2*b));
 }
 if(k!=0) {
 } else return w;
}
