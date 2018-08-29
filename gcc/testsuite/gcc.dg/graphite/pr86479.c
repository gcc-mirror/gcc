/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize -fnon-call-exceptions -fno-guess-branch-probability -fno-tree-loop-im" } */

__INTPTR_TYPE__ uf;

void
m7 (__INTPTR_TYPE__ *aw, __INTPTR_TYPE__ ws)
{
  __INTPTR_TYPE__ *e5 = &ws;

  if (ws < 1)
    {
      int cq = 0;

      while (cq < 1)
        {
          int *ng;
          int *ud;

          *e5 *= uf < 0;

          for (*ng = 0; *ng < 2; ++*ng)
            {
            }

          ws /= cq;
          *aw *= ws;

          for (*ud = 0; *ud < 2; ++*ud)
            {
            }
        }
    }

  if (ws < 2)
    e5 = &uf;

  *e5 = 0;
}
