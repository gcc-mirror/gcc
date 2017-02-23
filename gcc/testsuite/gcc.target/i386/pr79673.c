/* { dg-do compile } */
/* { dg-options "-O2" } */

void used(double x);
void usel(long x);
void test(int c)
{
  if (c)
    used(*((double __seg_gs *) 0));
  else
    usel(*((long __seg_gs *) 0));
}
