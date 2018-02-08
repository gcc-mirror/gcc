/* { dg-do compile { target powerpc*-*-* } }*/
/* { dg-options "-O1 -fno-split-wide-types -m32 -mcpu=e300c3" } */

int
nh (void)
{
}

long long int
si (void)
{
}

int
xf (int fg)
{
  int y5 = nh ();
  fg += !!y5 ? y5 : si ();
  return fg;
}
