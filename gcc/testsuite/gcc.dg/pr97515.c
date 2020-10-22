/* { dg-do compile } */
/* { dg-options "-O2" } */

int
e7 (int gg)
{
  int xe = 0;

  while (xe < 1)
    {
      int ui;

      ui = ~xe;
      if (ui == 0)
        ui = xe >> gg;

      xe %= !ui;
    }

  return xe;
}
