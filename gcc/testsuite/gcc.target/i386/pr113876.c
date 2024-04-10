/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O -mapxf -mpreferred-stack-boundary=3 -finstrument-functions -mcmodel=large" } */

void
bar (unsigned long *p)
{
  p[0] = 0;
  p[1] = 0;
  p[2] = 0;
}
