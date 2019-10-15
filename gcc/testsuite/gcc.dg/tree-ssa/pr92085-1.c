/* { dg-do compile } */
/* { dg-options "-O1 -fexceptions -fnon-call-exceptions -ftree-loop-vectorize -fno-tree-sink --param dse-max-alias-queries-per-store=2 -w" } */

void
di (int y9, int qw)
{
  if ((int) &y9 != 0)
    {
      int py;
      int **fq = &py;

      while (qw < 1)
        {
          if ((0 < (**fq ? **fq : (**fq = 1))) / (**fq = y9))
            ;

          ++qw;
        }
    }
}
