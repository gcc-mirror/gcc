/* PR rtl-optimization/82597 */
/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops" } */

int pb;

void
ch (unsigned char np, char fc)
{
  unsigned char *y6 = &np;

  if (fc != 0)
    {
      unsigned char *z1 = &np;

      for (;;)
        if (*y6 != 0)
          for (fc = 0; fc < 12; ++fc)
            {
              int hh;
              int tp;

              if (fc != 0)
                hh = (*z1 != 0) ? fc : 0;
              else
                hh = pb;

              tp = fc > 0;
              if (hh == tp)
                *y6 = 1;
            }
    }

  if (np != 0)
    y6 = (unsigned char *)&fc;
  if (pb != 0 && *y6 != 0)
    for (;;)
      {
      }
}
