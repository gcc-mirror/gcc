/* { dg-do compile } */

/* Check that __extension__ introduces intervening code.  */

extern void do_something (void);

void imperfect1 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)  /* { dg-error "not enough nested loops" } */
    {
      __extension__ ({
	  for (int j = 0; j < y; j++)  /* { dg-error "loop not permitted in intervening code" } */
	    do_something ();
	});
    }
}

void perfect1 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)  /* { dg-error "not enough nested loops" } */
    /* { dg-error "inner loops must be perfectly nested" "" { target *-*-*} .-1 } */
    {
      __extension__ ({
	  for (int j = 0; j < y; j++)  /* { dg-error "loop not permitted in intervening code" } */
	    do_something ();
	});
    }
}

/* Check that we don't barf on __extension__ in the inner loop body.  */
void imperfect2 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)
    for (int j = 0; j < y; j++)
      {
	__extension__ ({
	    do_something ();
	  });
      }
}

void perfect2 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)
    for (int j = 0; j < y; j++)
      {
	__extension__ ({
	    do_something ();
	  });
      }
}
