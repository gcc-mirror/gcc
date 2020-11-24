/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void kill (void);

void foo (unsigned int arg)
{
  int a = arg - 3;
  unsigned int b = 4;

  if (a < 0)
    {
      int x = 0x1 << arg;
      b = x;
    }

  if (b >=  5)
   kill ();
}

/* { dg-final { scan-tree-dump-not "kill" "evrp" } } */
