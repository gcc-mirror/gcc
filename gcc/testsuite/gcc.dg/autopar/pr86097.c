/* { dg-options "-O2 -ftree-parallelize-loops=2 -fno-tree-dce -Wno-aggressive-loop-optimizations" } */
int rp, vd;

void
p5 (int cd)
{
  while (cd != 0)
    {
      for (rp = 0; rp < 4; ++rp)
        for (vd = 0; vd < 1; ++vd)
          {
 g0:
            ;
          }

      ++rp;
    }

  while (rp < 2)
    {
      for (cd = 0; cd < 1; ++cd)
        for (rp = 1; rp != 0; ++rp)
          {
          }

      ++rp;
    }

  if (cd != 0)
    goto g0;
}
