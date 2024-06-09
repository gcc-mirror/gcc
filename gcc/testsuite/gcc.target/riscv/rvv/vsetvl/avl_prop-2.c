/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gc_zve32f -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

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

/* { dg-final { scan-assembler-times {vsetivli\tzero,\s*1} 2 } } */
