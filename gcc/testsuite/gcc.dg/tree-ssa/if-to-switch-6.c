/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized --param case-values-threshold=5" } */

int global;
int foo ();

int main(int argc, char **argv)
{
  if (argc >= 1 && argc <= 10)
    foo ();
  else if (argc == 12)
    {
      global += 1;
    }
  else if (argc == 13)
    {
      foo ();
      foo ();
    }
  else if (argc == 14)
    {
      foo ();
    }
  /* This will be removed with EVRP.  */
  else if (argc == 5)
    {
      global = 2;
    }
  /* This will be removed with EVRP.  */
  else if (argc >= 7 && argc <= 9)
    {
      global = 2;
    }

  else
    global -= 123;

  global -= 12;
  return 0;
}

/* { dg-final { scan-tree-dump-not "Condition chain" "iftoswitch" } } */
