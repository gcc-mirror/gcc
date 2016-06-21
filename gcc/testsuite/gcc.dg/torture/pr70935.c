/* { dg-do compile } */
/* { dg-options "-O3 -g" } */
/* { dg-require-effective-target ptr32plus } */

int d0, sj, v0, rp, zi;

void
zn(void)
{
  if (v0 != 0)
    {
      int *js, *r3;
      int pm, gc;

      for (gc = 0; gc < 1; ++gc)
        {
          sj = 1;
          while (sj != 0)
            ;
        }
      r3 = &pm;
      *js = (long)&gc;
ka:
      for (d0 = 0; d0 < 2; ++d0)
        {
          d0 = zi;
          if (zi)
            for (pm = 2; pm != 0; --pm)
              ;
        }
      while (*r3 != 0)
        {
          while (pm)
            ;
          ++r3;
        }
    }
  rp = 0;
  goto ka;
}
