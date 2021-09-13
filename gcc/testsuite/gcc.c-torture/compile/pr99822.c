/* PR target/99822 */
/* { dg-do assemble } */
/* { dg-require-effective-target int128 } */

int zt, bm, p5 = 1;

void __attribute__ ((cold))
l2 (unsigned long int hz)
{
  __int128 d9 = 0;
  unsigned long int *mg = hz ? &hz : (unsigned long int *) &d9;

  while (d9 < 1)
    {
      bm = bm > d9;
      bm = bm == (d9 = bm || hz);

      hz = 0x197000000;
      d9 = hz * hz;

      while (p5 < 1)
        {
          bm = ((hz = 3) ? zt : 0) > 0x1001;
          if (bm != 0)
            {
              __int128 *nd = (__int128 *) bm;

              *nd /= 3;
            }

          *mg = 0x1001;
          p5 -= *mg;
        }

      for (zt = 0; zt >= 0; zt += 2)
        d9 = 0;

      d9 += 2;
    }
}

