/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-mcpu=neoverse-v1 -O2 -fvect-cost-model=dynamic -fno-tree-scev-cprop" } */

int m, n;

void
foo (unsigned int x, short int y)
{
  if (m)
    for (;;)
      {
        ++m;
        while (m < 1)
          {
            n += m + x;
            ++m;
          }
      }

  for (;;)
    if (y)
      {
        ++x;
        if (x)
          for (y = 0; y < 75; y += 2)
            {
            }
      }
}

