/* { dg-do compile } */

/* This test case is expected to fail due to errors.  */

int f1 (int depth, int iter);
int f2 (int depth, int iter);
void do_something (void);

void s1 (int a1, int a2, int a3)
{
  int i, j, k;

#pragma omp for collapse(3)
  for (i = 0; i < a1; i++)
    {
      f1 (0, i);
      if (a1 < a2)
	{
	  int z = 0;
	  while (z < i)	/* { dg-error "loop not permitted in intervening code " } */
	    {
	      do_something ();
	      z++;
	    }
	  do		/* { dg-error "loop not permitted in intervening code " } */
	    {
	      do_something ();
	      z--;
	    } while (z >= 0);
	}
      for (j = 0; j < a2; j++)
	{
	  for (k = 0; k < a3; k++)
	    {
	      f1 (2, k);
	      f2 (2, k);
	    }
	  f2 (1, j);
	}
      if (a1 < a3)
	{
	  int z;
	  for (z = 0; z < i; z++)	/* { dg-error "loop not permitted in intervening code " } */
	    {
	      do_something ();
	    }
	}
      f2 (0, i);
    }
}
