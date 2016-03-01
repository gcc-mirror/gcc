/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 --param fsm-scale-path-blocks=1" } */

int func_81 (int);
int func_98 (int);

void
func_18 ( int t )
{
  unsigned l_889;
  int l_895 = 1;
  for (0; 1; ++l_889)
    {
      int t1 = 0;
      func_98 (0);
      func_98 (0);
      if (func_81 (1))
	{
	  int rhs = l_895;
	  if (rhs == 0)
	    rhs = 1;
	  if (1 & (t % rhs))
	    t1 = 1;
	}
      func_98 (t1);
      l_895 = 0;
    }
}

/* There should be a single if left.  */

/* { dg-final { scan-tree-dump-times "if" 1 "vrp1" } } */
