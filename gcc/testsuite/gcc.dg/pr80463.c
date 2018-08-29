/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-g -O2 -fvar-tracking-assignments -fselective-scheduling2 -ftree-loop-vectorize -fnon-call-exceptions -fno-tree-vrp -fno-gcse-lm -fno-tree-loop-im -fno-reorder-blocks-and-partition -fno-reorder-blocks -fno-move-loop-invariants -w" } */

short int t2;
int cd, aa, ft;

void
dh (void)
{
  int qs = 0;

  if (t2 < 1)
    {
      int bq = 0;

      while (bq < 1)
        {
        }

      while (t2 < 1)
        {
          if (t2 == 0)
            {
              bq = 0;
              cd = !!cd;
            }
          else
            {
              bq = 1;
              cd = bq > qs;
            }

          t2 += cd;
          bq = (t2 / qs) == bq;

          if (aa != ft)
            {
              qs %= 0;
              while (bq != 0)
                {
 ro:
                  ;
                }
            }

          ++t2;
        }

 ia:
      goto ro;
    }

  goto ia;
}
