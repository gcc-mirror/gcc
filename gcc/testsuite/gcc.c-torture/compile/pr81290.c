/* { dg-options "-funroll-loops -w" } */
int vz;

void
ms (int sw, int cm)
{
  for (vz = 0; vz < 19; ++vz)
    {
 fx:
      sw *= 2;
    }

  for (;;)
    {
      if (sw != 0)
        for (;;)
          {
          }
      if (1 / 0 && cm != 0)
        goto fx;
    }
}
