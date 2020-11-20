/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp2" } */

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

/* EVRP should be able to reduce this to a single goto when we can
 * revisit statements to try folding again based on changed inputs.
 * Until then, make sure its gone by ccp2.  */
 
/* { dg-final { scan-tree-dump-times "goto" 1 "ccp2" } } */
