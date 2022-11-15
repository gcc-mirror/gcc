/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-ch -fno-unswitch-loops -w -fdump-tree-loopdone-details" } */

void
t6 (int qz, int wh)
{
  int jl = wh;

  while (1.0 * qz / wh < 1)
    {
      qz = wh * (wh + 2);

      while (wh < 1)
        jl = 0;
    }

  while (qz < 1)
    qz = jl * wh;
}

/* { dg-final { scan-tree-dump-times "Replacing" 3 "loopdone"} } */
