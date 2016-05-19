/* PR tree-optimization/71031 */
/* { dg-do compile } */
/* { dg-options "-Os" } */

int zj;
int **yr;

void
nn (void)
{
  unsigned int od = 4;

  for (;;)
    {
      int lk;

      for (lk = 0; lk < 2; ++lk)
        {
          static int cm;

          zj = 0;
          if (od == 0)
            return;
          ++od;
          for (cm = 0; cm < 2; ++cm)
            {
              --od;
              **yr = 0;
            }
        }
    }
}
