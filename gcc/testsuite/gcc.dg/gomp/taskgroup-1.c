/* { dg-do compile } */

void
foo (int x)
{
  bad1:
  #pragma omp taskgroup
    goto bad1;			/* { dg-error "invalid branch" } */

  goto bad2;			/* { dg-error "invalid entry" } */
  #pragma omp taskgroup
    {
      bad2: ;
    }

  #pragma omp taskgroup
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }

  switch (x)			/* { dg-error "invalid entry" } */
  {
  #pragma omp taskgroup
    { case 0:; }
  }
}
