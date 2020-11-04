/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

int
e7 (int gg)
{
  int xe = 0;

  while (xe < 1)
    {
      int ui;

      ui = ~xe;
      if (ui == 0)
        ui = xe >> gg;

      xe %= !ui;
    }

  return xe;
}

/* EVRP should be able to reduce this to a single goto.  */
 
/* { dg-final { scan-tree-dump-times "goto" 1 "evrp" } } */
