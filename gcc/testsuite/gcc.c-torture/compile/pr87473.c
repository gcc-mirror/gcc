/* PR87473: SLSR ICE on hidden basis with |increment| > 1.  */
/* { dg-additional-options "-fno-tree-ch" } */

void
t6 (int qz, int wh)
{
  int jl = wh;

  while (1.0 / 0 < 1)
    {
      qz = wh * (wh + 2);

      while (wh < 1)
        jl = 0;
    }

  while (qz < 1)
    qz = jl * wh;
}
