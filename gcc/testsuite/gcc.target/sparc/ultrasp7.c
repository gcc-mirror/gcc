/* PR c/8281 */
/* Originator: TANIGUCHI Yasuaki <yasuaki@k8.dion.ne.jp> */

/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -mcpu=ultrasparc -fPIC" } */

static const double bp = 1.0, dp_l[] = { 0.0 };

double __ieee754_pow(double x, double y)
{
  union {
    int lo;
    double d;
  }uz;

  double y1,t1,p_h,t,z;
  double z_h,z_l,p_l;
  double t2,r,s,u,v,w;
  int i = 0;
 
  double s_h,t_h;
  double s2,s_l,t_l;
 
 
  v = 1.0/(v+bp);
  uz.d = s_h = s = u*v;
  uz.lo = 0;
  s_h = uz.d;
  uz.d = t_h;
  uz.lo = 3;
  t_h = uz.d;
  s_l = v*((u-s_h*t_h)-s_h*t_l);
  s2 = s*s;
  r = s2* s2* (1.1+s2*(1.2+s2*(1.3+s2*(1.4+s2*(1.5+s2*1.6)))));
  s2 = s_h*s_h;
  uz.lo = 0;
  t_h = uz.d;
  t_l = r-((t_h-3.0)-s2);
  v = s_l*t_h+t_l*s;
  p_l = v-(p_h-u);
  z_h = bp *p_h;
  z_l = bp*p_h+p_l*1.0+dp_l[i];
  t = (double)i;
  t1 = (((bp+z_l)+bp)+t);
  t2 = z_l-(((t1-t)-bp)-z_h);
  p_l = (y-y1)*t1+y*t2;
  z = p_l+p_h;

  return s*z;
}
