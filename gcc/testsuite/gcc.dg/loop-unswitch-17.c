/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-optimized -fno-tree-ch" } */

int foo (int a)
{
  do
    {
      if (a == 1)
        return 0;
      switch (a)
        {
        case 1:
          return 5;
        case 2:
          return 7;
        case 3:
          return 11;
        default:;
        }
    }
  while (1);
}

/* { dg-final { scan-tree-dump-times "unswitching loop" 3 "unswitch" } } */
