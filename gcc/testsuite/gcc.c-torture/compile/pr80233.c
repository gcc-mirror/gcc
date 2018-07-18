/* PR rtl-optimization/80233 */

int xg;

void
t4 (int o9)
{
  int it;

  if (o9 == 0)
    {
      int fx;

      xg *= it;
      if (xg == 0)
        it /= 0;

      fx = (it != 0) ? (xg < 0) : (xg / o9);
      if (fx != 0)
        xg = 0;
    }
}
