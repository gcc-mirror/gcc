/* { dg-skip-if "not yet" { c++ } } */

int i, j, k;
extern int foo (void);

void
f1 (void)
{
  #pragma acc parallel
  #pragma acc loop collapse (2)
  for (i = 0; i < 5; i++)
    ;					/* { dg-error "not enough perfectly nested" } */
  {
    for (j = 0; j < 5; j++)
      ;
  }
}

void
f2 (void)
{
  #pragma acc parallel
  #pragma acc loop collapse (2)
  for (i = 0; i < 5; i++)
    {
      {
	{
	  for (j = 0; j < 5; j++)
	    {
	    }
	}
      }
    }
}

void
f3 (void)
{
  #pragma acc parallel
  #pragma acc loop collapse (2)
  for (i = 0; i < 5; i++)
    {
      int k = foo ();			/* { dg-error "not enough perfectly nested" } */
      {
	{
	  for (j = 0; j < 5; j++)
	    {
	    }
	}
      }
    }
}

void
f4 (void)
{
  #pragma acc parallel
  #pragma acc loop collapse (2)
  for (i = 0; i < 5; i++)
    {
      {
	for (j = 0; j < 5; j++)
	  ;
	foo ();				/* { dg-error "collapsed loops not perfectly nested before" } */
      }
    }
}

void
f5 (void)
{
  #pragma acc parallel
  #pragma acc loop collapse (2)
  for (i = 0; i < 5; i++)
    {
      {
	for (j = 0; j < 5; j++)
	  ;
      }
      foo ();				/* { dg-error "collapsed loops not perfectly nested before" } */
    }
}

void
f6 (void)
{
  #pragma acc parallel
  #pragma acc loop collapse (2)
  for (i = 0; i < 5; i++)
    {
      {
	for (j = 0; j < 5; j++)
	  ;
      }
    }
  foo ();
}
