/* { dg-options "-O2 -mlong-double-128 -mabi=ibmlongdouble" } */

extern unsigned long int x;
extern float f (float);
extern __typeof (f) f_power8;
extern __typeof (f) f_power9;
extern __typeof (f) f __attribute__ ((ifunc ("f_ifunc")));
static __attribute__ ((optimize (1))) __typeof (f) *
f_ifunc (void)
{
  __typeof (f) *res = x ? f_power9 : f_power8;
  return res;
}
