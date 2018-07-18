/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -floop-nest-optimize" } */

struct
{
  int bz;
} od, ka[2];

int fw;

void
pc (void)
{
  for (od.bz = 0; od.bz < 2; ++od.bz)
    {
      ++fw;
      ka[0] = ka[1];
    }
}
