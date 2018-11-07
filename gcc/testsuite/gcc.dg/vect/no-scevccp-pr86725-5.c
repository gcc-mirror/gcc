/* { dg-do compile } */
/* { dg-additional-options "-O -w" } */

unsigned int foo;
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
          xe += qq;
        }
      while (qq-- > 0);
    }
  foo = wo;
  return xe;
}

/* { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" { target vect_int } } } */
