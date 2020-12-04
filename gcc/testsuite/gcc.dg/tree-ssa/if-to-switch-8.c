/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized --param case-values-threshold=5" } */

int global;
int global1;
int global2;
int global3;

int foo(int a, int b)
{
  int x = 0;
  for (unsigned i = 0; i < a; i++)
  {
    if (b == 1)
      global += 2;
    else if (a == 2)
      global = 123;
    else if (a == 3)
      global1 = 1234;
    else if (a == 10)
      global2 = 12345;
    else if (a == 1)
      global2 = 123456;
  }
}

/* { dg-final { scan-tree-dump-not "Condition chain" "iftoswitch" } } */
