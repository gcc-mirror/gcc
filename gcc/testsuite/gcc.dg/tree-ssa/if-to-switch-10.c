/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized --param case-values-threshold=5" } */

int global;
int foo ();

int main(int argc, char **argv)
{
  if (argc != 1)
    {
      if (argc != 2)
	{
	  if (argc == 3)
	    {
	      foo ();
	      foo ();
	    }
	  else if (argc == 4)
	    {
	      foo ();
	    }
	  else if (argc == 5)
	    {
	      global = 2;
	    }
	  else
	    global -= 123;
	}
      else
	{
	  global += 1;
	}
    }
  else
    foo ();

  
  global -= 12;
  return 0;
}

/* { dg-final { scan-tree-dump "Canonical GIMPLE case clusters: 1 2 3 4 5" "iftoswitch" } } */
/* { dg-final { scan-tree-dump "Condition chain with \[^\n\r]\* BBs transformed into a switch statement." "iftoswitch" } } */

