/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

double g;

int foo(char *flags, long len, long i, long steps)
{
  register long step, iter;

  if(*(flags + i))
    {
      step = i + i + 3;
      for(iter = i + step ; iter <= len ; iter += step)
	{
	  steps++;
	  *(flags + iter)=0;
	}
    }
   g = 5.0*(double)steps;

   return 0;
}

/* Don't expand iv {base+step, step}_loop into {base+x+y, step}_loop
   even if "step == x + y".  */
/* { dg-final { scan-tree-dump "Base:\\tstep_\[0-9\]* \\+ iter|Base:\\titer_\[0-9\]* \\+ step" "ivopts"} } */
