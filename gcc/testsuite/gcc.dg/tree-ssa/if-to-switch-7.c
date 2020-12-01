/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized" } */

int global;

int foo(int a)
{
  int x = 0;
  for (unsigned i = 0; i < a; i++)
  {
    if (a == 2)
    {
      global += 123;
      x = 1;
    }
    else if (a == 3)
      x = 2;
    else if (a == 10)
      x = 3;
  }

  return x;
}

/* { dg-final { scan-tree-dump-not "Condition chain " "iftoswitch" } } */
