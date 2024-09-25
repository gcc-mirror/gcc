/* { dg-do compile } */
/* { dg-additional-options "-O -w" } */

int
nr (unsigned int xe, unsigned int qqn)
{
  unsigned int oo, wo = 0;

  for (oo = 0; oo < 4; ++oo)
    {
      unsigned int qq = qqn;
      do
        {
          wo += 1;
          xe += wo;
        }
      while (qq-- > 0);
    }
  return xe;
}

/* { dg-final { scan-tree-dump "Unknown def-use cycle pattern" "vect" { target vect_int } } } */
/* { dg-final { scan-tree-dump-not "OUTER LOOP VECTORIZED" "vect" } } */
