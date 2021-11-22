/* { dg-do compile } */
/* { dg-options "-O -fstrict-aliasing -fdump-tree-lim2-details" } */

unsigned p;

void foo (float *q)
{
  for (int i = 0; i < 256; ++i)
    {
      if (p)
        {
          unsigned a = p;
          *(q++) = 1.;
          p = a + 1;
        }
    }
}

/* { dg-final { scan-tree-dump-times "Executing store motion" 1 "lim2" } } */
