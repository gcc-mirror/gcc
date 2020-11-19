/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void kill (void);

void foo (unsigned int arg)
{
  unsigned int C000003FE = 4;

  if (arg + 1 < 4)  // work for if (arg < 3)
     C000003FE = 0x1 << arg;

  if (C000003FE >= 5)
    kill ();
}

/* { dg-final { scan-tree-dump-not "kill" "evrp" } } */
