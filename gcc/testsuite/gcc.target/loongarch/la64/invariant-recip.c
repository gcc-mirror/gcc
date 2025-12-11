/* { dg-do compile } */
/* { dg-options "-Ofast -march=loongarch64 -mabi=lp64d -mrecip -mfrecipe -fdump-rtl-loop2_invariant " } */
/* { dg-final { scan-rtl-dump "Decided to move dependent invariant" "loop2_invariant" } } */

void
nislfv_rain_plm (int im, int km, float dzl[im][km], float rql[im][km],
                 float dt)
{
  int i, k;
  float con1, decfl;
  float dz[km], qn[km], wi[km + 1];

  for (i = 0; i < im; i++)
    {
      for (k = 0; k < km; k++)
        {
          dz[k] = dzl[i][k];
        }
      con1 = 0.05;
      for (k = km - 1; k >= 0; k--)
        {
          decfl = (wi[k + 1] - wi[k]) * dt / dz[k];
          if (decfl > con1)
            {
              wi[k] = wi[k + 1] - con1 * dz[k] / dt;
            }
        }
      for (k = 0; k < km; k++)
        {
          rql[i][k] = qn[k];
        }
    }
}
