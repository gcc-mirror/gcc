/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-linear" } */

/* This testcase was causing an ICE in building distance vectors because
   we weren't ignoring the fact that one of the induction variables
   involved in the dependence was outside of the loop.  */
extern int foo (int, int);
int
main (void)
{
  int a[50];
  int b[50];
  int i, j, k;
  for (i = 4; i < 30; i++)
    {
      for (j = 3; j < 40; j++)
	{
	  for (k = 9; k < 50; k++)
	    {
	      b[j] = a[i];
	      a[k] = b[i];
	    }
	}
    }
  foo (a[i], b[i]);
}
